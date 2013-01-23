(ns hangman.util-test
  (:use clojure.test
        hangman.util))

(deftest juxtcat-test
         (is (= ((juxtcat #(take 2 %) #(take 3 %)) 
                   [0 1 2 3 4 5])
                [0 1 0 1 2])))

(deftest divide-evenly-test
         (are [n m vec] (= (divide-evenly n m) vec)
              0  0  []
              10 0  []
              10 1  [10]
              10 2  [5 5]
              10 3  [3 3 4]
              10 4  [2 2 2 4]
              10 5  [2 2 2 2 2]
              10 6  [1 1 1 1 1 5]
              10 7  [1 1 1 1 1 1 4]
              10 8  [1 1 1 1 1 1 1 3]
              10 9  [1 1 1 1 1 1 1 1 2]
              10 10 [1 1 1 1 1 1 1 1 1 1])

         (is (thrown? java.lang.AssertionError (divide-evenly 1 -1)))
         (is (thrown? java.lang.AssertionError (divide-evenly -1 -1)))
         (is (thrown? java.lang.AssertionError (divide-evenly 1 2))))
