(ns hangman.bitsets-test
  (:use clojure.test
        hangman.util
        hangman.bitsets
        hangman.coll
        hangman.stats
        hangman.game)
  (:require [wall.hack :as wh])
  (:import GuessingStrategy
           GuessWord
           GuessLetter))

; LMAO if you try to test code which hides state in private fields and doesn't
; implement equals.
(defprotocol GuessValue
  "Equality for all!"
  (guess->value [this]))

(extend-protocol GuessValue
  GuessLetter
  (guess->value [this]
                (wh/field GuessLetter :guess this))

  GuessWord
  (guess->value [this]
                (wh/field GuessWord :guess this)))


(defn equiv-guesses
  "Because this algorithm is not deterministic, you can specify functions of
  appropriate target guesses in addition to characters and strings."
  [targets actuals]
  (every?
    (fn [[target actual]]
      (if (ifn? target)
        (target actual)
        (= target actual)))
    (zip-all targets actuals)))

(defn test-guesses!
  "Given a strategy and a game, ensures that the strategy produces the given
  series of guesses."
  [game strategy guesses]
  (is (equiv-guesses guesses
                     (map guess->value 
                          (:guesses (play! game strategy))))))

(deftest strategy-test
         (let [corpus ["CAB" "CAR" "CAT" "CUT" "CATS" "CROW" "CROWN"]
               s      (bitset-strategy corpus {:target-char-p 1/2})]
           (are [string guesses] (test-guesses! (game string) s guesses)
                "CAB"  [\T \B "CAB"]
                "CAR"  [\T \B "CAR"]
                "CAT"  [\T \U "CAT"]
                "CUT"  [\T #{\R \U} "CUT"]
                "CATS" [\T "CATS"]
                "CROW" [\T "CROW"]
                "CROWN" ["CROWN"]
                )))

(def example-words
  {"COMAKER"       25
   "CUMULATE"      9
   "ERUPTIVE"      5
   "FACTUAL"       9
   "MONADISM"      8
   "MUS"           25
   "NAGGING"       7
   "OSES"          5
   "REMEMBERED"    5
   "SPODUMENES"    4
   "STEREOISOMERS" 2
   "TOXICS"        11
   "TRICHROMATS"   5
   "TRIOSE"        5
   "UNIFORMED"     5
   })

(deftest full-test
         (let [corpus (mapv #(.toUpperCase %)
                                    (word-list "words.txt"))
               s      (bitset-strategy corpus {:cache-size 512})]
           (are [score string] (<= (:score (play! (game string 100) s))
                                  score)
                ; Three letter words is a pretty broad search space.
                ; but we get lucky
                4 "CAT")

           ; Compare to reference words.
           (let [words  (keys example-words)
                 scores (into {}
                              (map (fn [word]
                                     [word
                                      (:score (play! (game word 5) s))])
                             words))
                 deltas (into {}
                              (map (fn [k]
                                     [k (- (scores k) (example-words k))])
                                   words))]
             (prn deltas))

           ; Try 1000 random words.
           (let [scores (time (doall 
                                (pmap (fn [word]
                                        (:score (play! (game word 5) s)))
                                      (uniform-sample 1000 corpus))))]
             (prn scores)
             (prn :mean (float (apply mean scores))))))
