(ns hangman.util-test
  (:use clojure.test
        hangman.util))

(deftest peach-indexed-test
         (let [a (atom [])
               morsels [:a :b :c :d :e :f :g :h :i :j :k :l :m :o :p :q]]
           (peach-indexed #(swap! a conj [%1 %2]) morsels)
           (is (= (set @a)
                  (set (map-indexed #([%1 %2]) morsels))))))
