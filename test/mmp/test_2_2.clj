(ns mmp.test-2-2
  (:require [clojure.test :refer :all]
            [mmp.task2.task-2-2 :refer :all :as task2.2]))



(deftest integral-seq-test
  (testing "Sequenced integral function"
    ;f(x) = x
    (is (< (abs (- 12.5 ((task2.2/calc-integral-seq (fn [x] x) 0.01) 5))) 0.01))
    ;f(x) = 3/2 * x^2
    (is (< (abs (- 500.0 ((task2.2/calc-integral-seq (fn [x] (* (* x x) (/ 3 2))) 0.01) 10))) 0.01)))
  )