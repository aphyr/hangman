(ns hangman.stats-test
  (:use clojure.test
        hangman.stats))

(deftest character-set-test
         (is (= (set (character-set ["at \uffff" " 唐 "]))
                #{\a \t \uffff \唐 \space})))

(deftest character-counts-test
         (is (= (character-counts ["aa" "ab"])
                {\a 3 \b 1})))

(deftest character-occurrences-test
         (is (= (character-occurrences ["aa" "ab"])
                {\a 2 \b 1})))

(deftest mean-test
         (are [coll m] (= (apply mean coll) m)
              [5]            5
              [1 2 3]        2
              [1 2 3 1 1 -5] 1/2))

(deftest biggest-key-test
         (is (= :c (biggest-key {:a 2 :b -5 :c 10}))))

(deftest closest-key-test
         (is (= :b (closest-key {:a -1 :b 2 :c 10} 3))))

(deftest uniform-sample-test
         (let [naturals (iterate inc 0)
               sample   (uniform-sample 1000 10000 naturals)]
           ; No duplicates, in order
           (is (= sample (sort (set sample))))
           (is (= 1000 (count sample)))
           ; Basic uniformity :/
           (is (< 0    (first sample) 9000)
           (is (< 1000 (last sample) 10000)))))
