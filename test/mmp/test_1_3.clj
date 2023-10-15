(ns mmp.test_1_3
  (:require [clojure.test :refer :all]
            [mmp.task1.task-1-3 :refer :all :as task1.3]))


(deftest test-my-map
  (testing "my-map should apply given function to each element of the coll"
    (is (= (task1.3/my-map inc '(1 2 3 4 5)) '(2 3 4 5 6)))
    (is (= (task1.3/my-map dec '(1 2 3 4 5)) '(0 1 2 3 4)))
    (is (= (task1.3/my-map str '(a b c d e)) '("a" "b" "c" "d" "e")))
    (is (= (task1.3/my-map (fn [x] (* x x)) '(1 2 3 4 5)) '(1 4 9 16 25)))
    (is (= (task1.3/my-map identity '()) '())))
    (is (= (task1.3/my-map #(str "Hello " % "!" ) ["Ford" "Arthur" "Tricia"]) '("Hello Ford!" "Hello Arthur!" "Hello Tricia!")))
    (is (= (task1.3/my-map vector [[:a :b :c] [:d :e :f] [:g :h :i]])) '([:a :d :g] [:b :e :h] [:c :f :i])))

(deftest test_my_filter
  (testing "my-filter should return the items in coll for which (pred item) returns logical true")
  (is (= (task1.3/my-filter even? (range 10)) '(0 2 4 6 8)))
  (is (= (task1.3/my-filter odd? (range 10)) '(1 3 5 7 9)))
  (is (= (task1.3/my-filter #(> % 100) (range 95 105)) '(101 102 103 104)))
  (is (= (task1.3/my-filter #(> % 100) '()) '()))
  (is (= (task1.3/my-filter (fn [n] (= 0 (mod n 3))) (range 1 21)) '(3 6 9 12 15 18)))
  )