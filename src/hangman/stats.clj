(ns hangman.stats
  "Statistical functions for counting and sorting things."
  (:use [clojure.math.numeric-tower :only [abs]]))

(defn character-set
  "A list of all unique characters in a seq of strings."
  [words]
  (distinct (mapcat seq words)))

(defn character-counts
  "Counts appearances of characters in a seq of strings. Yields a map of
  characters to counts."
  [words]
  (reduce (fn [dist char]
            (assoc dist char
                   (inc (get dist char 0))))
          {}
          (mapcat seq words)))

(defn character-occurrences
  "Given a seq of strings, returns a map of characters to the number of strings
  that character appears in."
  [words]
  ; Rapid cycling of immutable hashmaps was a significant bottleneck in the
  ; idiomatic clojure variant of this function, so I've opted for a mutable
  ; approach.
  (let [dist    (java.util.HashMap.)
        charset (java.util.HashSet.)]
    (doseq [word words]
      (doseq [c word]
        (.add charset c))
      (doseq [c charset]
        (.put dist c (inc (get dist c 0))))
      (.clear charset))
    (into {} dist)))

(defn mean
  "Mean value of xs."
  [& xs]
  (/ (reduce + xs)
     (count xs)))

(defn biggest-key
  "Given a map, picks the key with the highest value."
  [m]
  (when-not (empty? m)
    (apply max-key m (keys m))))

(defn closest-key
  "Given a map, picks the key with the value closest to target"
  [m target]
  (first (first
           (sort-by (fn [[k v]] (abs (- v target)))
                    m))))

(defn uniform-sample
  "A uniformly random sample of a sequence in linear time. Gets close to, but
  doesn't guarantee, a sample of n elements. Lazy, constant space, preserves
  order. If total is provided, will consume up to [total] elements from the
  given sequence. If total is omitted, uses (count s). Example:

  ; Pick a single element from a, b, c, and d.
  (uniform-sample [:a :b :c :d]
  ; => :b

  ; Pick up to 2 elements from a, b, c, and d.
  (uniform-sample 2 [:a :b :c :d])
  ; => [:a :d]

  ; Pick up to five numbers from the first thousand integers.
  (uniform-sample 5 1000 (iterate inc 0))
  ; => [54 643 780 901]"
  ([s] (first (uniform-sample 1 s)))
  ([needed s] (uniform-sample needed (count s) s))
  ([needed total [elem & more :as s]]
   (when (and (not (empty? s)) (< 0 needed))
     (lazy-seq
       (if (<= needed (rand total))
         (uniform-sample needed (dec total) more)
         (cons elem
               (uniform-sample (dec needed) (dec total) more)))))))
