(ns hangman.set
  "Set algebra functions.
  
  Set expressions are lists comprised of an operator followed by one or more
  expressions. Any unrecognized expression is preserved by set-algebraic
  transformations. The operators are:
  
  :intersect  ⋂
  :union      ⋃
  :subtract   ∖
  :complement ∁ 
  :empty      ∅
  :universe   U

  :intersect, :union, and :subtract are n-ary."
  (:require clojure.set)
  (:use [clojure.core.match :only [match]]))

(defn normalized-rank
  "The rank of an expression determines its position in normalized form."
  [expr]
  (if (coll? expr)
    (case (first expr)
      :intersect  10
      :union      11
      :subtract   12
      :complement 13
      100)
    (case expr
      :empty 0
      :universe 1
      100)))

(defn normalize
  "Converts an expression to an equivalent one in normalized form.  Used by
  (optimize expr) for pattern-matching.

  Reorders the arguments of commutative operators into a normalized form.
  Removes duplicate arguments to :intersect, :union, and :subtract."
  [expr]
  (if (sequential? expr)
    (let [[operator & args] expr]
      (case operator
        :intersect (cons operator (sort-by normalized-rank (distinct args)))
        :union     (cons operator (sort-by normalized-rank (distinct args)))
        :subtract  (cons operator 
                         (cons (first args) 
                               (sort-by normalized-rank (distinct
                                                          (rest args)))))
        expr))
    expr))

(declare optimize-pass)

(defn optimize-de-morgan
  "De Morgan's law for reducing complements."
  [expr]
  (case (first expr)
    :intersect (if (every? #(= :complement (first %)) (rest expr))
                 [:complement (cons :union (map second (rest expr)))]
                 expr)
    :union     (if (every? #(= :complement (first %)) (rest expr))
                 [:complement (cons :intersect (map second (rest expr)))])))

(defn optimize-subtract
  "Various subtraction optimizations."
  [expr]
  (let [[_ a & more] expr
        ; Subtracting empty sets does nothing.
        more (remove :empty more)]
    (cond
      ; Unary subtraction
      (empty? more) a

      ; Empty subtraction
      (= :empty a) :empty

      ; Subtracting from an intersection
      (and (sequential? a)
           (= :intersect (first a))
           (some (set (rest a)) more))
      :empty

      ; Subtracting one's self, or the universe
      (some #{a :universe} more) :empty

      ; Flatten subtrahend unions, and optimize-pass everything.
      :else
      (cons :subtract 
            (cons (optimize-pass a)
                  (map optimize-pass
                       (mapcat (fn [s]
                                 (if (and (sequential? s)
                                          (= :union (first s)))
                                   (rest s)
                                   [s]))
                                 more)))))))

(defn optimize-complement
  "Complement laws reducing to :empty or :universe."
  [expr]
  (match [expr]
         [([:complement :universe] :seq)]
         :empty

         [([:complement :empty] :seq)]
         :universe

         [([:intersect ([:complement x] :seq) & more] :seq)]
         (if (some #{x} more)
           :empty
           expr)

         [([:union ([:complement x] :seq) & more] :seq)]
         (if (some #{x} more)
           :universe
           expr)

         [_]
         expr))

(defn complement->subtraction
  "Converts set complement operations to subtraction, where possible."
  [expr]
  (if (sequential? expr)
    (let [[op & sets] expr]
      (case op
        ; (:intersect x (:complement y)) -> (:subtract x y)
        :intersect (let [s (group-by (fn [s]
                                       (and (sequential? s)
                                            (= :complement (first s))))
                                     sets)]
                     (if (and (s true) (s false))
                       (cons :subtract
                             (cons (cons :intersect (s false))
                                   (map second (s true))))
                       expr))
        expr))
    expr))

(defn optimize-pass
  "A single optimizer pass."
  [expr]
  (let [expr (-> expr
               (normalize)
               (optimize-complement)
               (complement->subtraction))]
    (match [expr]
           ; Involution
           [([:complement ([:complement x] :seq)] :seq)]
           (optimize-pass x)

           ; Unary union
           [([:union x] :seq)]
           (optimize-pass x)

           ; Unary intersection
           [([:intersect x] :seq)]
           (optimize-pass x)

           ; De Morgan's laws
           [([:intersect ([:complement _] :seq)
                         ([:complement _] :seq) & _] :seq)]
           (optimize-de-morgan expr)

           [([:union ([:complement _] :seq)
                     ([:complement _] :seq) & _] :seq)]
           (optimize-de-morgan expr)

           ; Various subtraction laws
           [([:subtract & more] :seq)]
           (optimize-subtract expr)

           ; Nested union
           [([:union ([:union & x] :seq) & y] :seq)]
           (optimize-pass (concat [:union] x y))

           ; Nested intersection
           [([:intersect ([:intersect & x] :seq) & y] :seq)]
           (optimize-pass (concat [:intersect] x y))

           ; Identity laws
           [([:union :empty & xs] :seq)]
           (optimize-pass (cons :union xs))

           [([:intersect :universe & xs] :seq)]
           (optimize-pass (cons :intersect xs))

           ; Domination laws
           [([:union :universe & _] :seq)]
           :universe

           [([:intersect :empty & _] :seq)]
           :empty

           ; Absorption laws
           [([:intersect ([:union & inner] :seq) x] :seq)]
           (if (some #{x} inner)
             x
             expr)

           [([:union ([:intersect & inner] :seq) x] :seq)]
           (if (some #{x} inner)
             x
             expr)

           ; Complement children
           [([:complement x] :seq)]
           [:complement (optimize-pass x)]

           ; Everything else
           [x] x)))

(def optimize-pass-limit
  "How many optimizer passes will we allow?"
  5)

(defn optimize
  "Takes a set-algebraic expression and reorganizes it to minimize the number
  of operations. Runs through several optimization passes."
  [expr]
  (loop [i 0
         expr expr]
    (when (< i optimize-pass-limit)
;      (prn "Pass" i expr)
      (let [expr' (optimize-pass expr)]
        (if (= expr expr')
          expr'
          (recur (inc i) expr'))))))

(defprotocol EvaluableSet
  "Allows the evaluation of set-algebraic expressions over objects. Functions
  which end in ! may (optionally) mutate their first argument in place.
  For example, (union! a b) returns a Set which is the union of a and b, but
  may, depending on implementation, make destructive changes to a."
  (clone [a]        "Returns a copy of a.")
  (union! [a b]     "Destructive union.")
  (intersect! [a b] "Destructive intersection.")
  (subtract! [a b]  "Destructively subtract b from a."))

(extend-protocol EvaluableSet
  ; A basic implementation for testing
  clojure.lang.PersistentHashSet
  (clone       [a]   a)
  (union!      [a b] (clojure.set/union a b))
  (intersect!  [a b] (clojure.set/intersection a b))
  (subtract!   [a b] (clojure.set/difference a b)))

(defn evaluate*
  "Evaluates without optimization."
  [expr]
  (if (satisfies? EvaluableSet expr)
    expr
    (let [[operator a & bs] expr]
      (reduce (case operator
                :union     union!
                :intersect intersect!
                :subtract  subtract!
                :else      (throw (RuntimeException. 
                                    (str "Unknown set operator operator"
                                         operator))))
              ; Make a copy of the first argument, since we'll mutate it.
              (if (satisfies? EvaluableSet a)
                (clone a)
                (evaluate* a))
              (map evaluate* bs)))))

(defn evaluate
  "Given a set-algebraic expression, optimizes and evaluates it. Uses
  controlled mutability internally, where available. Does not mutate any sets
  it receives. Return values, however, are *not* guaranteed to be safely
  mutable, i.e. you may get the same sets back if no copies were necessary."
  [expr]
  (evaluate* (optimize expr)))
