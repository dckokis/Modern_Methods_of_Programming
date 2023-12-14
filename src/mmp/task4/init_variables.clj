(ns mmp.task4.init-variables
  (:require
    [mmp.task4.logical-operations :refer :all]
    [mmp.task4.dnf-rules :refer :all]
    ))

(declare init-expression-variables)

(defn init-rules [var value]
  "Set of rules to initialize given variable"
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] value)]
    [(fn [expr] (conjunction? expr))
     (fn [expr]
       (apply conjunction
              (map #(init-expression-variables var value %) (operation-args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr]
       (apply disjunction
              (map #(init-expression-variables var value %) (operation-args expr))))]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (init-expression-variables var value (second expr))))]
    [(fn [expr] (or (variable? expr) (constant? expr)))
     (fn [expr]
       expr)]))

(defn init-expression-variables
  "Initialize variables in given expression"
  [var val expr]
  (apply-rules expr (init-rules var val)))

(defn dnf-initialized
  "Construct DNF for expression with initialized var"
  [expr var val]
  (->>
    expr
    dnf
    (init-expression-variables var val)
    dnf))