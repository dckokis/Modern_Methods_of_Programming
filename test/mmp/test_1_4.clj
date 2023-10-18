(ns mmp.test_1_4
  (:require [clojure.test :refer :all]
            [mmp.task1.task-1-4 :refer :all :as task1.4]))


(deftest test-create-string-list
  (testing "create-string-list should return all permutations of given length from given list,
   where two same elements are nor placed together"
    (is (= (task1.4/create-string-list '("a" "b" "c") 2) '("ab" "ac" "ba" "bc" "ca" "cb")))
    (is (= (task1.4/create-string-list '("a" "b" "c") 3) '("aba" "abc" "aca" "acb" "bab" "bac" "bca" "bcb" "cab" "cac" "cba" "cbc")))
    (is (= (task1.4/create-string-list '("aaa" "b" "c") 2) '("aaab" "aaac" "baaa" "bc" "caaa" "cb")))
    (is (= (task1.4/create-string-list '() 2) '()))
    (is (= (task1.4/create-string-list '("a" "b") -2) '()))))
