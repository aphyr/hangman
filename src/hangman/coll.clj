(ns hangman.coll
  "Functions over collections."
  (:use [hangman.util :only [divide-evenly]]))

(defn each-indexed-nth
  "Calls (f index element), presumably for side effects, with each element of
  coll, over the range of indexes [start, end). Uses nth to avoid extra copies
  of indexed datatypes."
  ([f coll start end]
   (when (< start end)
     (f start (nth coll start))
     (recur f coll (inc start) end))))

(defn each-indexed-seq
  "Calls (f index element), presumably for side effects, with each element of
  coll, over the range of indexes [start, end). Uses recur with (first) and
  (rest) for O(n) iteration over non-indexed sequences."
  ([f coll start end]
   (loop [f     f
          coll  (drop start coll)
          start start
          end   end]
     (when (< start end)
       (f start (first coll))
       (recur f (rest coll) (inc start) end)))))

(defn each-indexed
  "Calls (f index element), presumably for side effects, with each element of
  coll, over the range of indexes [start, end). Uses -nth for vectors, and -seq
  for other collections."
  ([f coll]
   (each-indexed f coll 0 (count coll)))
  ([f coll start end]
   (if (vector? coll)
     (each-indexed-nth f coll start end)
     (each-indexed-seq f coll start end))))

(defn splitv
  "Partitions a vector v into n consective vectors, in order."
  [n v]
  (if (zero? n)
    '()
    (let [splits (reductions + (divide-evenly (count v) n))
          ranges (partition 2 1 (cons 0 splits))]
      (map (partial apply subvec v) ranges))))

(defn zip-all*
  "zip-all, but without varargs."
  [seqs]
  (let [head (map first seqs)]
    (when (some identity head)
      (lazy-seq
        (cons head (zip-all* (map rest seqs)))))))

(defn zip-all
  "Zip sequences together. As long as the longest sequence."
  [& seqs]
  (zip-all* seqs))
