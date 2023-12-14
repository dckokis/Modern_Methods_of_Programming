(ns mmp.task4.main
  (:require
    [mmp.task4.dnf-rules :refer :all]
    [mmp.task4.logical-operations :refer :all]
    [mmp.task4.init-variables :refer :all]))

(defn -main []
  ;x + y
  (println (dnf (disjunction (variable :x) (variable :y))))
  ;x + y = 1, (x = 1)
  (println
    (dnf-initialized (disjunction (variable :x) (variable :y)) (variable :x) constant-true))
  ;!(!(x->y) + z) + x + z * x + !x = 1 (т.к. есть тафтология !x + x = 1)
  (println
    (dnf
      (disjunction
        (negation
          (disjunction
            (negation (implication (variable :x) (variable :y)))
            (variable :z)))
        (variable :x)
        (conjunction (variable :z) (variable :x))
        (negation (variable :x))))))
