(ns hangman.game
  "Create, evaluate, and play hangman games. Games are mutable!"
  (:import HangmanGame
           HangmanGame$Status))

(def won HangmanGame$Status/GAME_WON)
(def lost HangmanGame$Status/GAME_LOST)
(def keep-guessing HangmanGame$Status/KEEP_GUESSING)

(defn game
  "Create a new game around a word. Defaults to 5 wrong guesses."
  ([word]
   (game word 5))
  ([word guesses]
   (HangmanGame. word guesses)))

(defn guess-letter
  "Returns a Guess for a letter."
  [letter]
  (GuessLetter. letter))

(defn guess-word
  "Returns a Guess for a word."
  [word]
  (GuessWord. word))

(defn score
  "The score of a game."
  [^HangmanGame game]
  (.currentScore game))

(defn won?
  "A winner is you!"
  [^HangmanGame game]
  (= (.gameStatus game) HangmanGame$Status/GAME_WON))

(defn lost?
  "Is the game lost?"
  [^HangmanGame game]
  (= (.gameStatus game) HangmanGame$Status/GAME_LOST))

(defn keep-guessing?
  "Can we keep guessing in the game?"
  [^HangmanGame game]
  (= (.gameStatus game) HangmanGame$Status/KEEP_GUESSING))

(defn known-chars
  "A sequence of known [index, character] pairs in a game."
  [^HangmanGame game]
  (second
    (reduce (fn [[i known] c]
              [(inc i)
               (if (= c HangmanGame/MYSTERY_LETTER)
                 known
                 (conj known [i c]))])
            [0 []]
            (.getGuessedSoFar game))))

(defn excluded-chars
  "A sequence of [index, character] pairs we know aren't in a game."
  [^HangmanGame game]
  (let [n (.getSecretWordLength game)]
    (mapcat (fn [c]
              (map (fn [i] [i c])
                   (range n)))
            (.getIncorrectlyGuessedLetters game))))

(defn game-space
  "A map representing the space of potential words. Keys are:

  :length    The length of the word.
  :included  A list of [index, character] pairs known to be in the word.
  :excluded  A list of characters not in the word."
  [^HangmanGame game]
  ; Note that UnmodifiableSet hasheq doesn't place nice with core.cache. :(
  {:length   (.getSecretWordLength game)
   :included (known-chars game)
   :excluded (.getIncorrectlyGuessedLetters game)})

(defn game-space-string
  "A string representing the space of potential words. We're spending a huge
  amount of time comparing keys in the strategy cache, so choosing the most
  compact objects as keys really matters.
  
  This cut running time for 1000 words from 38 seconds to 20 seconds."
  [^HangmanGame game]
  (let [; These characters are reserved by the unicode spec
        ; for internal use, and are guaranteed not to be
        ; character codepoints.
        < \ufffe
        > \uffff
        s (game-space game)]
    (str 
      < (:length s) >
      < (apply str (sort (:excluded s))) >
      < (apply str (map (fn [[i c]] (str < i > < c >)) (sort (:included s)))) >)))

(defn guess!
  "Makes a guess using strategy, and returns the guess. Mutates game. Returns
  nil when no more guesses are possible."
  [^HangmanGame game ^GuessingStrategy strategy]
  (when (keep-guessing? game)
    (let [guess (.nextGuess strategy game)]
      (.makeGuess guess game)
      guess)))

(defn play!
  "Plays a game of hangman using a strategy, and returns a map like:
  
  {:guesses   A sequence of Guess's produced by the strategy.
   :game      The game itself
   :won?      Whether the strategy won
   :lost?     Whether the strategy lost
   :score     The final score}

  Mutates the given game."
  [^HangmanGame game ^GuessingStrategy strategy]
  (let [guesses (doall 
                  (take-while identity
                              (repeatedly #(guess! game strategy))))]
    {:guesses guesses
     :game    game
     :won?    (won? game)
     :lost?   (lost? game)
     :score   (score game)}))
