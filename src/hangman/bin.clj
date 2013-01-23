(ns hangman.bin
  "I help your jar be executable!"
  (:gen-class)
  (:use [hangman.util    :only [word-list]]
        [hangman.stats   :only [mean uniform-sample]]
        [hangman.game    :only [game play!]]
        [hangman.bitsets :only [bitset-strategy]]))

(def logger (agent nil))
(defn log-print
  [_ & things]
  (apply println things))
(defn log
  [& things]
  (apply send-off logger log-print things))

(defn help
  []
  (log
    "Usage: hangman <corpus-file> [word-file [word-file ...]] [options]

    corpus-file is a list of words, one per line, which are candidates for play.
    word-file is a list of words, one per line, which shall be played.

    If no word-file is given, plays n randomly words from the corpus.

    Options:

    -h        Help
    -v        Verbose: more juicy stats
    -n        Number of words to play
    -t  5     Number of bad guesses allowed.
    -c  512   Number of cached character distributions
    -s  65536 Number of words to sample at each move
    -p  0.7   Aim to reduce the game space to this factor at each move.")) 

(defn now
  []
  (/ (System/nanoTime) 1000000000))

(defn flags
  [flags]
  (first 
    (reduce (fn [[parsed prev] arg]
              (if-let [flags (re-matches #"^-(.*)" arg)]
                ; Flags!
                (let [flags (map (comp keyword str)
                                 (second flags))]
                  [(into parsed (map (fn [f] [f true]) flags))
                   (last flags)])

                ; Value!
                [(assoc parsed prev arg) prev]))
            [{} nil]
            flags)))

(defn -main
  ([] (help))
  ([corpus & args]
   (let [[word-files opts] (split-with #(not (re-matches #"^-.+" %)) args)
         opts          (flags opts)
         help?         (:h opts)
         verbose?      (:v opts)
         n             (when (:n opts)
                         (Long. (:n opts)))
         tries         (Long. (:t opts 5))
         cache-size    (Long. (:c opts 512))
         sample-size   (Long. (:s opts 65536))
         target-char-p (Float. (:p opts 0.7))]

     (when help? (help))

     ; Read corpus
     (let [corpus   (mapv #(.toUpperCase %)
                          (word-list corpus))

           ; Create strategy
           strategy (if verbose?
                      (do
                        (log "Indexing" (count corpus) "words...")
                        (time
                          (bitset-strategy corpus
                                           {:cache-size    cache-size
                                            :sample-size   sample-size
                                            :target-char-p target-char-p})))
                      (bitset-strategy corpus
                                       {:cache-size    cache-size
                                        :sample-size   sample-size
                                        :target-char-p target-char-p}))

           ; Read target words
           words  (if (empty? word-files)
                    corpus
                    (map #(.toUpperCase %)
                         (mapcat word-list word-files)))
           
           ; Select random subsample if necessary
           sample (cond
                    n
                    (do
                      (log "Sampling" n "words...")
                      (uniform-sample n words))

                    (empty? word-files)
                    (do
                      (log "Playing all" (count words) "words in corpus...")
                      words)

                    :else
                    (do
                      (log "Playing " (count words) "words from" word-files)
                      words))

           ; Play games
           ; Retains the head, including all guesses. 
           ; DEAL WITH IT.
           t0    (now)
           games (doall
                   (pmap (fn [word]
                           (let [outcome (play! (game word tries) strategy)]
                             (when verbose?
                               (locking log
                                 (log (:score outcome) "\t" word)))
                             (dissoc outcome :game)))
                         sample))
           t1    (now)

           ; PHASE 5: BUREAUCRACY
           dt     (- t1 t0)
           n      (count games)
           total  (reduce + (map :score games))
           mscore  (apply mean (map :score games))
           mmoves  (apply mean (map (comp count :guesses) games))
           rate   (/ n dt)
           wins   (count (filter :won? games))
           losses (count (filter :lost? games))]


       ; Log stats
       (when verbose? (log))
       (log (str "Completed " n " games in " (float dt) " seconds ("
                     (float rate) " games/sec)"))
       (log (str "Won       " wins   " (" (float (/ wins   n)) ")"))
       (log (str "Lost      " losses " (" (float (/ losses n)) ")"))
       (log (str "Average game took " (float mmoves) " guesses"))
       (log (str "Total score " total " (" (float mscore) "/game)"))
       
       ; Cleanup
       (await logger)
       (shutdown-agents)))))
