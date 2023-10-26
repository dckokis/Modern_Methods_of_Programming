(ns mmp.test-2-1
  (:require [clojure.test :refer :all]
            [mmp.task2.task-2-1 :refer :all :as task2.1]))


(deftest integral-test
  (testing "Not memoized integral function"
    (is (< (abs(- 12.5 (task2.1/integral (fn [x] x)  0.01 5))) 0.01))
    (is (< (abs (- 500.0 (task2.1/integral (fn [x] (* (* x x) (/ 3 2))) 0.01 10))) 0.01)))
  )
(deftest integral-mem-test
  (testing "Memoized integral function"
    (is (< (abs(- 12.5 (task2.1/integral-mem (fn [x] x) 0.01 5))) 0.01))
    (is (< (abs (- 500.0 (task2.1/integral-mem (fn [x] (* (* x x) (/ 3 2))) 0.01 10))) 0.01)))
  )
