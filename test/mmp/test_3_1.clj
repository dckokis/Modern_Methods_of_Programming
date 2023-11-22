(ns mmp.test_3_1
  (:require [clojure.test :refer :all]
            [mmp.task3.task-3-1 :refer :all :as task3.1]))


(deftest test_pfilter
  (testing "pfilter is a parallel impl of filter"
    (is (= (task3.1/pfilter even? 2 (range 10)) '(0 2 4 6 8)))
    (is (= (task3.1/pfilter  odd? 2 (range 10)) '(1 3 5 7 9)))
    (is (= (task3.1/pfilter  #(> % 100) 2 (range 95 105)) '(101 102 103 104)))
    (is (= (task3.1/pfilter  #(> % 100) 2 '()) '()))
    (is (= (task3.1/pfilter  (fn [n] (= 0 (mod n 3))) 2 (range 1 21)) '(3 6 9 12 15 18)))))
