(ns hangman.parallel
  "Functions to support parallelism."
  (:use [hangman.coll :only [each-indexed]]
        [hangman.util :only [divide-evenly]]))

(defn processors
    "How many available processors on this runtime?"
    []
    (.availableProcessors (Runtime/getRuntime)))

(defn prn-locked
  "Prn, but only allows one thread to print a line at a time. Locks the prn
  function itself."
  [& args]
  (locking prn
    (apply prn args)))

; "Why," you ask, "are you going to all this trouble, Kyle? Couldn't you just
; partitition and pmap for a cheap fork-join?" Yes, yes we could, but doing it
; a.) without retaining the head, b.) with indexes and c.) in a cache-aware way
; requires a lot of superfluous index math and temporary objects, plus
; destructuring bind in each function. It's reasonably fast, but we're talking
; about CPU-bound functions so time is of the essence.

(defn peach-indexed
  "Run CPU-bound functions over collections for their UNCANNY SIDE EFFECTS.
  It's like map-indexed mixed with dorun mixed with pmap mixed with GRATUITOUS
  AMOUNTS OF PARALLELISM. Your functions are going to be EMBARASSINGLY PARALLEL
  and they're going to WIN AT RACE CONDITIONS and get their data DEPORTED
  VIA TLB SHOOTDOWNS.
  
  Calls (f 0 (first coll)), (f 1 (second coll)), and so forth, but in an
  undetermined order. Defaults to one thread per processor."
  ([f coll]
   (peach-indexed f (processors) coll))
  ([f threads coll]
   (let [length (count coll)
         threads (min threads length)
         ; Divide coll's indices into *threads* distinct ranges.
         splits (reductions + (divide-evenly (count coll) threads))
         ranges (partition 2 1 (cons 0 splits))]
       ; Run each-indexed over each range, in parallel.
       (dorun (pmap (partial apply each-indexed f coll) ranges)))))
