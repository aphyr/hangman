(ns hangman.util
  "You are standing in a garage, with an open toolbox before you.
  
  It is dark. You are likely to be eaten by a cons."
  (:require [clojure.java.io :as io]
            [clojure.core.cache :as cache])
  (:use     [clojure.math.numeric-tower :only [abs]]))

(defn word-list
  "A vector of words in a file."
  [file]
  (-> file
    (io/file)
    (io/reader)
    (line-seq)
    (vec)))

(defn juxtcat
  "Takes one or more functions. Returns a function which takes a variable
  number of args, and returns the concatenation of applying each fn to args,
  left-to-right. Think mapcat but with juxt.
  
  ((juxtcat #(take 2 %) #(take 3 %)) [0 1 2 3 4 5])
  ; => (0 1 0 1 2)"
  [& fns]
  (let [j (apply juxt fns)]
    (fn [& args]
      (apply concat (apply j args)))))

; "Why," you ask, "are you going to all this trouble, Kyle? Couldn't you just
; partitition and pmap for a cheap fork-join?" Yes, yes we could, but doing it
; a.) with indexes and b.) in a cache-aware way requires a lot of superfluous
; index math and temporary objects, plus destructuring bind in each function.
; It's reasonably fast, but we're talking about CPU-bound functions so time is
; of the essence.
(defn divide-evenly
  "Divides an integer n into a vector of m roughly equal integers."
  [n m]
  (assert (<= 0 m n))
  (if (= 0 m)
    []
    (concat
      (replicate (dec m) (quot n m))
      [(- n    (* (dec m) (quot n m)))])))

