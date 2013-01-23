(ns hangman.set-test
  (:use clojure.test
        hangman.set))

(deftest normalize-test
         (are [a b] (= b (normalize a))
              :x
              :x

              [:and :a]
              [:and :a]
              
              [:and :b :a :c]
              [:and :b :a :c]

              [:union :a :universe :b]
              [:union :universe :a :b]
              ))

(deftest optimize-test
         (are [a b] (= b (optimize a))
              ; Identity
              :x
              :x

              ; Involution
              [:complement [:complement :x]]
              :x
             
              ; Unary intersection
              [:intersect :a]
              :a
              
              ; Unary union
              [:union :a]
              :a

              ; Unary subtraction
              [:subtract :a]
              :a

              ; Normal intersection
              [:intersect :a :b]
              [:intersect :a :b]

              ; Nested intersection
              [:intersect [:intersect :a :b] :c]
              [:intersect :a :b :c]
              
              [:intersect [:intersect [:intersect :a :b] :c] :d]
              [:intersect :a :b :c :d]

              [:intersect :a [:intersect :b]]
              [:intersect :b :a]

              ; Normal union
              [:union :a :b]
              [:union :a :b]

              ; Nested union
              [:union [:union :a :b] :c]
              [:union :a :b :c]

              [:union [:union [:union :a :b] :c] :d]
              [:union :a :b :c :d]

              [:union :a [:union :b]]
              [:union :b :a]

              ; Normal subtraction
              [:subtract :a :b]
              [:subtract :a :b]

              ; Identity laws
              [:union :a :empty :b]
              [:union :a :b]

              [:intersect :a :universe :b]
              [:intersect :a :b]
              
              ; Domination laws
              [:union :a :universe :b]
              :universe
              
              [:intersect :a :empty :b]
              :empty
             
              ; Complement laws
              [:intersect :a [:complement :a]]
              :empty

              [:union :a [:complement :a]]
              :universe

              [:complement :universe]
              :empty

              [:complement :empty]
              :universe

              ; Idempotent laws
              [:intersect :a :a :b :a]
              [:intersect :a :b]
              
              [:union :b :a :b :a]
              [:union :b :a]

              [:subtract :a :b :c :b]
              [:subtract :a :b :c]

              ; Absorption laws
              [:union :a [:intersect :a :b]]
              :a
              
              [:union :a [:intersect :b :c :a]]
              :a

              [:intersect :a [:union :a :b]]
              :a
              
              [:intersect :a [:union :b :c :a]]
              :a

              ; De Morgan's laws
              [:intersect [:complement :a] [:complement :b]]
              [:complement [:union :a :b]]

              [:intersect [:complement :a] [:complement :b] [:complement :c]]
              [:complement [:union :a :b :c]]

              [:union [:complement :a] [:complement :b]]
              [:complement [:intersect :a :b]]
              
              [:union [:complement :a] [:complement :b] [:complement :c]]
              [:complement [:intersect :a :b :c]]

              ; Subtraction
              [:subtract :x :universe]
              :empty

              ; Intersection with a complement -> subtraction
              [:intersect :x [:complement :y]]
              [:subtract :x :y]
              
              [:intersect [:complement :a] :x [:complement :b] :y]
              [:subtract [:intersect :x :y] :a :b]

              ; Subtraction with a union -> subtraction
              [:subtract :a [:union :b :c]]
              [:subtract :a :b :c]

              ; Bigger fish
              [:union 
               [:complement [:intersect :a [:complement :a]]]
               [:complement [:intersect :a :b]]]
              :universe
              
              ; A hangman query 
              ; Oh yeah, that's what we came here for, wasn't it?
              [:intersect :a
                          [:intersect :b :c]
                          [:complement
                          [:union :d :e :f]]]
              [:subtract [:intersect :b :c :a] :d :e :f]
              ))

(deftest evaluate-test
         (are [a b] (= b (evaluate a))
              #{}
              #{}
              
              #{1 2 3} 
              #{1 2 3}
              
              [:union #{1} #{2} #{3}]
              #{1 2 3}

              [:intersect #{1} #{2 1} #{6 7 1}]
              #{1}

              [:union [:intersect #{2 3} #{3 4}] #{5 6}]
              #{3 5 6}

              [:subtract #{4 5 6} #{5} #{5 7}]
              #{4 6}

              ; An hangman query
              [:intersect #{1 2 3 4} #{2 3 4 5}
               [:complement
                [:union
                 #{1}
                 #{2}]]]
              #{3 4}
              ))

(deftest performance
         ; Optimizes roughly 16,000 queries/second, and evaluates roughly 7200
         ; queries/second on my box. Pretty abysmal in general terms, but more
         ; than sufficient for playing hangman. I'm inclined to fire it up in
         ; yourkit, but I've already spent too much time on this tangent. ;-)
         ;
         ; Why even bother? Because in an *actual* search engine, the query
         ; optimizer is trivially parallelizable--but actually fetching indices
         ; means going to the network or disk and dealing with possible
         ; synchronization issues. It's worth spending the extra time up-front,
         ; especially if your optimizer patterns can be readily memoized.
         (time (dotimes [i 10000]
                 (evaluate [:intersect #{1 2 3 4} #{2 3 4 5}
                            [:complement
                             [:union
                              #{1}
                              #{2}]]]))))
