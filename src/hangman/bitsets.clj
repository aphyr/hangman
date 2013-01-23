(ns hangman.bitsets
  "A hangman AI built around an immutable, lockless in-memory search engine.
  Everything is concurrency-safe, unless otherwise noted.
  
  ## Search
  
  Our goal is to rapidly narrow the set of words (the 'game space') for a given
  game. There are three pieces of information which vary with game state.

  1. The length of the word is known, which restricts the entire corpus to words
     of a particular length.

  2. Each character position, if known, allows us to restrict the search space
     to all words having that character in that position.

  3. Invalid characters, once guessed, exclude every word which contains those
     characters.

  Because this game has a decently sized (~ (chars ^ word length)) state space,
  we can't reasonably precompute indexes for every potential game state.
  
  However, we *can* compute separate candidate sets for each (position, char)
  possibility, and intersect and subtract them to quickly reduce the space.
  There are only (chars * word length) distinct terms to construct. For this
  problem's corpus, that's (at max) 728 terms.

  What might an index look like? Since this game has a *fixed corpus* we can
  simply enumerate every word. We'll use integers for length terms, and [0 \\c]
  as the term for 'words with a c in the first position'.

  [bathing cats crows call fools]
  [   0      1    1    1     0  ]  [0 c]  first letter is c
  [   1      1    0    1     0  ]  [1 a]  second letter is a
  [   0      1    0    1     0  ]  4      4 letter words
  [   0      1    0    1     0  ]         intersection of the above

  Bitsets provide fast intersection and have trivial, bijections with the
  corpus. We can map back to real words quickly.

  Constructing queries for games is a simple transformation. For grins (because
  hey, this is a search engine), I've written a set-algebraic query optimizer
  and evaluation algorithm in hangman.set. Since these are bitsets all
  operations have roughly the same cost, and we simply try to reduce the number
  of intersections performed.

  ## Choosing characters

  The search engine allows us to find words in the game space. The dual
  problem is to evolve the game space by choosing letters and words (which
  in turn drives the evolution of the query).

  Our approach is to probabilistically sample a subset of the search space, and
  determine the probability that a given character will bisect the search space
  in some way. Because hangman's scoring is nonlinear, binary search is
  actually less efficient in terms of score. A tunable parameter :target-char-p
  controls whether we choose characters which are present in larger or smaller
  portions of the game space.

  This sampling process is still linear in the number of samples, and consumes
  the bulk of our processing time. Because the probability distribution of
  words fed to our algorithm is uniform, we can assume the dynamics of the
  underlying phase space to be *stable*: a least-frequently-used cache rounds
  out our approach.

  # A note for the reader

  The instructions were to 'have fun', so I went crazy. You'll find
  impenetrable functional programming, bizarre performance optimizations,
  infinite sequences, caching, dynamic programming, pattern matching,
  statistical methods, parallelism, laziness, useless type polymorphism, etc.

  In the real world, yes, I would use the closed-form solution. ;-)

  # While we're on the topic

  Another way to represent this problem space is to consider each word a unit
  vector in a (length * character-cardinality)-dimensional vector space. Each
  move in the game intersects an additional set of hypersurfaces and
  hypervolumes through this space. We can apply linear algebra techniques to
  compute optimal hypersurfaces given some information about the statistical
  distribution of elements through the space; reducing to a set of polynomials
  might work.
  
  In practice, I think this approach boils down to 'throw WEKA at the problem'."
  (:use hangman.cache
        [hangman.util     :only [juxtcat]]
        [hangman.coll     :only []]
        [hangman.stats    :only [uniform-sample
                                 closest-key
                                 character-occurrences]]
        [hangman.parallel :only [processors
                                 peach-indexed]]
        [hangman.game     :only [game-space-string 
                                 game-space
                                 known-chars 
                                 excluded-chars]]
        [hangman.set      :only [optimize
                                 evaluate
                                 EvaluableSet]])
  (:require [clojure.core.cache :as cache])
  (:import (java.util BitSet)
           GuessingStrategy
           HangmanGame))

; Strictly speaking this polymorphism isn't necessary, but I've been
; experimenting with several types of indexes here so it cuts down on typing.
(defprotocol ResultSet
  "A set of keys. Includes controlled mutability, but concurrency-safe."
  (conj-result! [this k]     "Add a result to the set, destructively.")
  (results      [this]       "A sequence of results in the set.")
  (clone        [this]       "Make a copy of this index for destructive
                             changes.")
  (cardinality  [this]       "How many results are in the index?")
  (negate!      [this]       "In-place set complement.")
  (intersect!   [this other] "Remove all elements not present in other.")
  (union!       [this other] "Add all elements in other.")
  (subtract!    [this other] "Remove all elements in other."))

(extend-protocol ResultSet
  BitSet
  (conj-result! [this k]
                (locking this
                  (.set this k)))

  (results [this]
           (take-while #(<= 0 %)
                       (rest
                         (iterate #(.nextSetBit this (inc %)) -1))))

  (cardinality [this]
               (.cardinality this)))

(extend-protocol EvaluableSet
  BitSet
  (clone [this] (.clone this))
  (intersect! [a b] (.and a b)    a)
  (union!     [a b] (.or a b)     a)
  (subtract!  [a b] (.andNot a b) a))

(defn ensure-resultset!
  "Given an atom -> an index, and a key, ensures index contains a resultset for
  key and returns that resultset. Uses (generator) to create resultsets where
  none exist."
  [index k generator]
  (if-let [resultset (get @index k)]
    ; No need for CaS.
    resultset
    (-> ; Add the new index atomically.
      (swap! index (fn [index]
                     (if (get index k)
                       index
                       (assoc index k (generator)))))
      ; And return the resultset
      (get k))))

(defn index-by
  "Returns an index for words.

  f          an indexer function, which returns a list of terms for each word.
  generator  a function which returns a new resultset.
  threads    the number of threads to use
  words      the set of words to index.
  
  In a few experiments on my quad-core box, performance maxes out around 4
  threads. Scales sublinearly, at least on this small corpus: four threads on a
  quad-core cuts running time to ~40% of a single thread. This is to be
  expected given the small cardinality of the corpus: threads have to spend a
  fair bit of time fighting over the same letter positions.
  
  This particular indexing function optimizes word read locality at the cost of
  contention on indexes. An easy way to show this is by sorting the word list
  prior to indexing: threads conflict less on the word-length resultsets and
  throughput rises. A lockfree bitset (hellloooo GPU) would probably be the
  next step for improving this, but I'm running short on time.
 
  Another possibility would be to allocate small bitsets for each portion of
  the corpus, merge them into unified bytebuffers, and convert *that* into a
  bitset again. No locks (except for the merge phase which is fast), and better
  resultset locality, but that's a somewhat less readable solution."
  ([f generator threads words]
   (let [index (atom {})]
     (peach-indexed
       (fn worker [i word]
         (doseq [k (f word)]
           (-> (ensure-resultset! index k generator)
             (conj-result! i))))
       threads
       words)
     @index)))

(defn length-indexer
  "An indexing function which returns the number of characters in the word."
  [^java.lang.String word]
  (list (.length word)))

(defn char-indexer
  [word]
  "An indexing function which returns a list of [position, character] pairs in
  a word."
  (doall (map-indexed (fn [i c] [i c]) word)))

(defn index
  "Constructs an index for a set of words, using length and character indexers.
  Threads controls how many threads are used for indexing."
  ([words] (index words (processors)))
  ([words threads]
   (index-by (juxtcat length-indexer
                      char-indexer)
             #(BitSet. (count words))
             threads
             words)))

(defn words-in
  "A seq of all the words in an index."
  [index words]
  (map words (results index)))

(defn game-space-set
  "Returns a set-algebraic representation of the game space over an index."
  [^HangmanGame game index]
  [:intersect (index (.getSecretWordLength game))
              (cons :intersect (map index (known-chars game)))
              [:complement
                (cons :union (keep identity
                                   (map index (excluded-chars game))))]])

(defn character-dist*
  "Sample a resultset for its character occurrences. Since this part of our
  search requires linear scans and is CPU-bound, we fork-join parallelize when
  the result set is larger than sample-size. Returns [sampled-count
  distribution]"
  [strategy resultset]
  (let [results (results @resultset)
        sample  (map (:words strategy)
                     (uniform-sample (:sample-size strategy) results))
        dists   (character-occurrences sample)]
    [(count sample) dists])) 

(defn character-dist
  "Returns the distribution (possibly cached) of character occurrences in a
  game. Defers to character-dist when cached results are not available."
  [strategy ^HangmanGame game resultset]
  (let [k       (game-space-string game)]
    (or (cache-fetch! (:cache strategy) k)
        (cache-miss! (:cache strategy) k 
                     (character-dist* strategy resultset)))))

(defn next-word
  "Choose a word from the resultset."
  [strategy ^HangmanGame game resultset]
  (let [words (map (:words strategy)
                   (results @resultset))]
    (first (remove (set (.getIncorrectlyGuessedWords game)) words))))

(defn guess
  "Uses a resultset to make a guess about a game. Has a reference to a
  resultset, which may not be deref'd if the cache allows us to skip
  it."
  [strategy ^HangmanGame game resultset]
  (let [[n dist] (character-dist strategy game resultset)]
    (if (= 1 n)
      ; There's (at most) one word in the result set.
      (GuessWord. (next-word strategy game resultset))

      ; Otherwise, remove any used characters from the distribution.
      (-> (apply dissoc dist (.getAllGuessedLetters game))
        ; And choose the letter closest to our target probability.
        (closest-key (* n (:target-char-p strategy)))
        (GuessLetter.)))))

; A guessing strategy using a bitset index.
(defrecord BitSetStrategy [words index cache sample-size target-char-p]
  GuessingStrategy
      (nextGuess [this game]
                 ; We may not have to actually search the space, if the
                 ; cache is available.
                 (let [resultset (delay
                                   (evaluate (game-space-set game index)))]
                   (guess this game resultset))))

(defn bitset-strategy
  "A GuessingStrategy using bitset indexes and a cache. Takes a list of words,
  and options:

  :sample-size, which controls the number of words from the game space which
  are analyzed to determine the optimal discriminating letter.
  
  :cache-size, the number of distinct game states maintained in our internal
  cache. Higher per-character entropy in the word space will perform better
  with broader caches. 512 is a reasonable size.

  :target-char-p, from most accurate (1) to fastest (0.5), to useless (0).
  
  Ideally, we should choose a letter from each set of possible words which
  appears in some fraction of those words.

  If you're interested in the *fastest* algorithm, the ideal choice is 1/2--it
  maximally bisects the search space at every turn. However, because there's a
  significant score penalty for choosing a wrong word, p=1/2 has notably
  suboptimal scores on the sample corpus with max_wrong_guesses 5.

  A reasonable tradeoff between speed and accuracy in these circumstances is
  the default 7/10.

  Could do an adaptive algorithm to find this dynamically for a corpus and
  game, but we're already waaay out of scope."
  ([words]
   (bitset-strategy words {}))
  ([words opts]
  (let [index (index words)
        sample-size   (get opts :sample-size 65536)
        cache-size    (get opts :cache-size 512)
        target-char-p (get opts :target-char-p 7/10)
        ; A cache of character occurrences. Since our workload distribution is
        ; stable over time and biased towards the early stages of games, a
        ; least-used cache is appropriate."
        cache (atom (cache/lu-cache-factory {} :threshold cache-size))]
    (BitSetStrategy. words index cache sample-size target-char-p))))
