(ns sbrian.combinatorics.test.core-tests
  (:use clojure.test)
  (:use sbrian.combinatorics.core))

(deftest test1
  (is ( = (n-choose-k-avoid-s-of-t 1 1 0 0) 1)))

(deftest test2
  (is ( = (n-choose-k-avoid-s-of-t 2 1 1 1) 1)))

(deftest test3
  (is ( = (n-choose-k-avoid-s-of-t 4 3 1 1) 3)))

(deftest test4
  (is ( = (n-choose-k-avoid-s-of-t 10 5 1 1) 126)))

(deftest test5
  (is ( = (n-choose-k-avoid-s-of-t 10 5 2 2) 56)))

(deftest test6
  (is ( = (n-choose-k-avoid-s-of-t 10 5 2 1) 112)))



