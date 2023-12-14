(ns mmp.task4.logical-operations)

(def constant-true
  "Represents logical true"
  (list :true))

(def constant-false
  "Represents logical false"
  (list :false))

(defn constant-false?
  "Check if value of given expression is false"
  [expr]
  (= (first expr) :false))

(defn constant-true?
  "Check if value of given expression is true"
  [expr]
  (= (first expr) :true))

(defn constant
  "Describe constant"
  [expr]
  (cond
    (constant-true? expr) constant-true
    (constant-false? expr) constant-false))

(defn constant?
  "Check if given expression is constant"
  [expr]
  (or (constant-true? expr)
      (constant-false? expr)))

(defn variable
  "Describe variable"
  [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn variable-name
  "Get variable name"
  [v]
  (second v))

(defn variable?
  "Check if given expression is variable"
  [expr]
  (= (first expr) :var))

;Проверка равенства переменных (нужно для удаления дублей)
(defn same-variables?
  "Check if given variables are same by name"
  [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))

(defn conjunction
  "Describe conjunction"
  [expr & rest]
  (cons :conjunction (cons expr rest)))

(defn conjunction?
  "Check if given expression is conjunction"
  [expr]
  (= (first expr) :conjunction))

(defn disjunction
  "Describe disjunction"
  [expr & rest]
  (cons :disjunction (cons expr rest)))

(defn disjunction?
  "Check if given expression is disjunction"
  [expr]
  (= (first expr) :disjunction))

(defn negation
  "Describe negation"
  [expr & rest]
  (cons :negation (cons expr rest)))

;Проверка на отрицание
(defn negation?
  "Check if given expression is negation"
  [expr]
  (= (first expr) :negation))

(defn implication
  "Describe implication"
  [expr & rest]
  (cons :implication (cons expr rest)))

(defn implication?
  "Check if given expression is implication"
  [expr]
  (= (first expr) :implication))

(defn operation-args
  "Get arguments of operation"
  [expr]
  (rest expr))