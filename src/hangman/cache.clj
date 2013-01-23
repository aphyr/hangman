(ns hangman.cache
  "A few helpers for clojure.core.cache"
  (require [clojure.core.cache :as cache]))

(defn lu-cache
  "A least-used cache."
  [size]
  (cache/lu-cache-factory {} :threshold size))

; I didn't expect this, but immutable clojure.core.caches with the STM
; outperform pretty much every java object cache I've tried, and by a large
; margin at that. Still haven't found a sane lock-free java cache.
(defn cache-fetch!
  "Given an atom pointing to a clojure.core.cache, fetches a key k."
  [cache k]
  (-> (swap! cache
             (fn update [c]
               (if (cache/has? c k)
                 (cache/hit c k)
                 c)))
    (get k)))

(defn cache-miss!
  "Given an atom pointing to a clojure.core.cache, a missed key, and the
  corresponding value, swaps it with (cache/miss cache k v), and returns
  value."
  [cache k v]
  (swap! cache cache/miss k v)
  v)
