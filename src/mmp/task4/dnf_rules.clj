(ns mmp.task4.dnf-rules
  (:require
    [mmp.task4.logical-operations :refer :all]))

(declare simplify-complex-operations)
(declare simplify-negation)
(declare simplify-distributivity)
(declare simplify-inner-binary-operations)
(declare simplify-binary-operations-with-constant)

(defn apply-rules
  "Apply set of rules to logical expression"
  [expr rules]
  ((some
     (fn [rule]
       (if ((first rule) expr)
         (second rule)
         false))
     rules)
   expr))

(def complex-operations-rules
  "Rules to simplify complex operations
  1. x -> y = !x + y
  2. (logical expr) * (logical expr)
  3. (logical expr) + (logical expr)
  4. !(logical expr)
  vars and constants keeps unmodified
  "
  (list
    ;x -> y = !x + y
    [(fn [expr] (implication? expr))
     (fn [expr]
       (simplify-complex-operations
         (disjunction
           (negation (simplify-complex-operations (first (operation-args expr))))
           (simplify-complex-operations (second (operation-args expr))))))]
    ;(logical expr) * (logical expr)
    [(fn [expr] (conjunction? expr))
     (fn [expr]
       (apply conjunction (map simplify-complex-operations (operation-args expr))))]
    ;(logical expr) + (logical expr)
    [(fn [expr] (disjunction? expr))
     (fn [expr]
       (apply disjunction (map simplify-complex-operations (operation-args expr))))]
    ;!(logical expr)
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (simplify-complex-operations (second expr))))]
    ;variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    ;constant
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]))

(defn simplify-complex-operations
  "Apply rules to simplify complex operations"
  [expr]
  (apply-rules expr complex-operations-rules))

(def negation-rules
  "Rules to simplify negations
  1. !(x*y) = !x + !y
  2. !(x+y) = !x * !y
  3. !!x = x
  4. !1 = 0
  5. !0 = 1
  6. (logical expr) * (logical expr)
  7. (logical expr) + (logical expr)
  8. !(logical expr)
  vars and constants keeps unmodified
  "
  (list
    ;!(x*y) = !x + !y
    [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
     (fn [expr]
       (simplify-negation
         (apply disjunction (map #(negation %) (operation-args (second expr))))))]
    ;!(x+y) = !x * !y
    [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
     (fn [expr]
       (simplify-negation
         (apply conjunction (map #(negation %) (operation-args (second expr))))))]
    ;!!x = x
    [(fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (simplify-negation (first (operation-args (second expr)))))]
    ;!1 = 0
    [(fn [expr] (and (negation? expr) (constant-true? (first (operation-args expr)))))
     (fn [expr] constant-false)]
    ;!0 = 1
    [(fn [expr] (and (negation? expr) (constant-false? (first (operation-args expr)))))
     (fn [expr] constant-true)]
    ;(logical expr) * (logical expr)
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map simplify-negation (operation-args expr))))]
    ;(logical expr) + (logical expr)
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map simplify-negation (operation-args expr))))]
    ;!(logical expr)
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (simplify-negation (second expr))))]
    ;variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    ;constant
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]))

(defn simplify-negation
  "Apply rules to simplify negation"
  [expr]
  (apply-rules expr negation-rules))

(def distributivity-rules
  "Rules to simplify distributions
  1. x * (y + z) = (x * y) + (x * z)
  2. (y + z) * x = (y * z) + (z * x)
  3. (logical expr) * (logical expr)
  4. (logical expr) + (logical expr)
  5. !(logical expr)
  vars and constants keeps unmodified
  "
  (list
    ;x * (y + z) = (x * y) + (x * z)
    [(fn [expr] (and (conjunction? expr) (disjunction? (nth expr 2))))
     (fn [expr]
       (simplify-distributivity
         (disjunction
           (conjunction
             (first (operation-args expr))
             (first (operation-args (second (operation-args expr)))))
           (conjunction
             (first (operation-args expr))
             (second (operation-args (second (operation-args expr))))))))]
    ;(y + z) * x = (y * z) + (z * x)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
     (fn [expr]
       (simplify-distributivity
         (disjunction
           (conjunction (first (operation-args (first (operation-args expr))))
                        (second (operation-args expr)))
           (conjunction (second (operation-args (first (operation-args expr))))
                        (second (operation-args expr))))))]
    ;(logical expr) * (logical expr)
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map simplify-distributivity (operation-args expr))))]
    ;(logical expr) + (logical expr)
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map simplify-distributivity (operation-args expr))))]
    ;!(logical expr)
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (simplify-distributivity (second expr))))]
    ;variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    ;constant
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]))

(defn simplify-distributivity
  "Apply rules to simplify distributions"
  [expr]
  (apply-rules expr distributivity-rules))

(def nested-binary-operations-rules
  "Rules to simplify nested binary operations"
  (list
    [(fn [expr] (and (conjunction? expr) (some conjunction? (operation-args expr))))
     (fn [expr]
       (let [conj #(some (if (conjunction? %) % nil) (operation-args expr))
             conj-args (operation-args conj)]
         (simplify-inner-binary-operations
           (apply conjunction
                  (concat (remove #(= % conj) (operation-args expr))
                          conj-args)))))]
    [(fn [expr] (and (disjunction? expr) (some disjunction? (operation-args expr))))
     (fn [expr]
       (let [disj (some #(if (disjunction? %) % nil) (operation-args expr))
             disj-args (operation-args disj)]
         (simplify-inner-binary-operations
           (apply disjunction
                  (concat (remove #(= % disj) (operation-args expr)) disj-args)))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr]
       (apply conjunction (map simplify-inner-binary-operations (operation-args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr]
       (apply disjunction (map simplify-inner-binary-operations (operation-args expr))))]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (simplify-inner-binary-operations (second expr))))]
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]))

(defn simplify-inner-binary-operations
  "Apply rules to simplify nested binary operations"
  [expr]
  (apply-rules expr nested-binary-operations-rules))

(def binary-operations-with-constant-rules
  "Rules to simplify expression contains binary operations with constants
  1. x * 1 * (logical_expr) = x * (logical_expr)
  2. x * 0 * (logical_expr) = 0
  3. x + 1 + (logical_expr) = 1
  4. x + 0 + (logical_expr) = x + (logical_expr)
  "
  (list
    ; x * 1 * (logical_expr) = x * (logical_expr)
    ; x * 0 * (logical_expr) = 0
    [(fn [expr] (and (conjunction? expr) (some constant? (operation-args expr))))
     (fn [expr]
       (if (=
             (some #(when (constant? %) %) (operation-args expr))
             constant-false)
         constant-false
         (apply conjunction (remove #(= % constant-true) (operation-args expr)))))]
    ; x + 1 + (logical_expr) = 1
    ; x + 0 + (logical_expr) = x + (logical_expr)
    [(fn [expr] (and (disjunction? expr) (some constant? (operation-args expr))))
     (fn [expr]
       (if (=
             (some #(when (constant? %) %) (operation-args expr))
             constant-true)
         constant-true
         (apply disjunction
                (map simplify-binary-operations-with-constant
                     (remove #(= % constant-false) (operation-args expr))))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr] expr)]
    [(fn [expr] (disjunction? expr))
     (fn [expr]
       (apply disjunction
              (map simplify-binary-operations-with-constant (operation-args expr))))]
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]
    [(fn [expr] (and (negation? expr) (variable? (first (operation-args expr)))))
     (fn [expr] expr)]
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]))

(defn simplify-binary-operations-with-constant
  "Apply rules to simplify expression contains binary operations with constants"
  [expr]
  (apply-rules expr binary-operations-with-constant-rules))

(defn simplify-single-disjunction
  "Simplify disjunction with 1 arg"
  [expr]
  (if (disjunction? expr)
    (if (> (count (operation-args expr)) 1)
      expr
      (second expr))
    expr))

(defn simplify-single-conjunction
  "Simplify conjunction with 1 arg"
  [expr]
  (if (conjunction? expr)
    (if (> (count (operation-args expr)) 1)
      expr
      (second expr))
    expr))

(defn check-negation-variable-or-constant
  "Check if negation is applied to variable or constant and returns result of negation.
  Returns name or value otherwise"
  [x]
  {:pre [(or (variable? x) (constant? x)
             (and (negation? x)
                  (or (variable? (first (operation-args x))) (constant? (first (operation-args x))))))]}
  (if (or (variable? x) (constant? x))
    x
    (first (operation-args x))))

(defn simplify-contradictions-conjunction
  "Simplify conjunction with contradictions"
  [expr]
  (let [args-list (operation-args expr)
        no-negations-list (map (fn [elem] (check-negation-variable-or-constant elem)) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-false
      expr)))

(defn simplify-conjunction [expr]
  (let [result (if (or (constant? expr) (variable? expr)
                       (and (negation? expr) (variable? (first (operation-args expr)))))
                 expr
                 (simplify-contradictions-conjunction
                   (apply conjunction (distinct (operation-args expr)))))]
    (simplify-single-conjunction result)))

(defn simplify-contradictions-disjunction
  "Simplify disjunction with contradictions"
  [expr]
  (let [args-list (operation-args expr)
        no-negations-list (map #(if (negation? %) (second %) %) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-true
      (distinct expr))))

(defn simplify-disjunction [expr]
  (->>
    (apply disjunction
           (map simplify-conjunction
                (if (or (constant? expr) (variable? expr)
                        (and (negation? expr) (variable? (first (operation-args expr)))))
                  expr
                  (operation-args expr))))
    simplify-contradictions-disjunction))

(defn simplify
  "1. If exps is const or var do nothing
   2. If expr is conjunction - delete duplicates and contradictions
   3. If expr is disjunction - delete duplicates and contradictions, simplify inner conjunctions
   4. If remains disjunction with 1 arg - simplify it"
  [expr]
  (let [result (cond
                 (constant? expr) expr
                 (variable? expr) expr
                 (negation? expr) expr
                 (conjunction? expr) (simplify-conjunction expr)
                 (disjunction? expr) (simplify-disjunction expr))]
    (simplify-single-disjunction result)))

(defn dnf
  "Construct DNF of given expression according to logical rules
   1. Get rid of all logical operations contained in the formula, replacing them with basic ones: conjunction, disjunction, negation
   2. Replace the negation sign relating to the entire expression with negation signs relating to individual variable statements based on the formulas:
      !(x * y) = !x + !y
      !(x + y) = !x * !y
   3. Get rid of double negations
   4. Apply, if necessary, distributive properties to the operations of conjunction and disjunction and remove nesting
   5. remove cases like x * 1 * z = x * z"
  [expr]
  (->>
    expr
    simplify-complex-operations
    simplify-negation
    simplify-distributivity
    simplify-inner-binary-operations
    simplify
    simplify-binary-operations-with-constant
    simplify))