(ns hangman.coll-test
  (:use clojure.test
        hangman.coll))

(deftest each-indexed-test
         (testing "vectors"
                  (let [a (atom [])]
                    (each-indexed #(swap! a conj [%1 %2])
                                  '(:a :b :c :d :e)
                                  1 3)
                    (is (= @a [[1 :b] [2 :c]]))))

         (testing "lists"
                  (let [a (atom [])]
                    (each-indexed #(swap! a conj [%1 %2])
                                  [:a :b :c :d :e]
                                  1 3)
                    (is (= @a [[1 :b] [2 :c]])))))

(deftest splitv-test
         (are [n b] (= b (splitv n [1 2 3 4]))
              0 []
              1 [[1 2 3 4]]
              2 [[1 2] [3 4]]
              3 [[1] [2] [3 4]]
              4 [[1] [2] [3] [4]]))

(deftest zip-all-test
         (are [a b] (= b (apply zip-all a))
              [[1 2] [:a :b]]  [[1 :a] [2 :b]]
              [[] [1 2]]       [[nil 1] [nil 2]]))
