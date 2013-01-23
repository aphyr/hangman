(ns hangman.util-test
  (:use clojure.test
        hangman.util))

(deftest cache-test
         (let [c (atom (lu-cache 2))]
           (is (= nil    (cache-fetch! :cat)))
           (is (= :meow) (cache-miss!  :cat :meow))
           (is (= :meow  (cache-fetch! :cat)))
           (is (= :meow  (cache-fetch! :cat)))
           (is (= :bark  (cache-miss!  :dog :bark)))
           (is (= :moo   (cache-miss!  :cow :moo)))
           (is (= #{:cat :cow} (keys @c)))))
