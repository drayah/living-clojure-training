(ns problems.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [problems.core :as c]))

(deftest replicator
  (is (= '(1 1 2 2 3 3) (c/replicator [1 2 3] 2)))
  (is (= '(:a :a :a :a :b :b :b :b) (c/replicator [:a :b] 4)))
  (is (= '(4 5 6) (c/replicator [4 5 6] 1)))
  (is (= '([1 2] [1 2] [3 4] [3 4]) (c/replicator [[1 2] [3 4]] 2)))
  (is (= [44 44 33 33] (c/replicator [44 33] 2))))

(deftest fibonacci
  (is (= '(1 1 2) (c/fib 3)))
  (is (= '(1 1 2 3 5 8) (c/fib 6)))
  (is (= '(1 1 2 3 5 8 13 21) (c/fib 8))))

(deftest fact-reduce
  (is (= 1 (c/fact-reduce 1)))
  (is (= 6 (c/fact-reduce 3)))
  (is (= 120 (c/fact-reduce 5)))
  (is (= 40320) (c/fact-reduce 40320)))

(deftest half-truth
  (is (= false (c/half-truth false false)))
  (is (= true (c/half-truth true false)))
  (is (= false (c/half-truth true)))
  (is (= true (c/half-truth false true false)))
  (is (= false (c/half-truth true true true)))
  (is (= true (c/half-truth true true true false))))

(deftest gcd
  (is (= 2 (c/gcd 2 4)))
  (is (= 5 (c/gcd 10 5)))
  (is (= 1 (c/gcd 5 7)))
  (is (= 33 (c/gcd 1023 858))))

(deftest pwr
  (is (= 256 ((c/pwr 2) 16) ((c/pwr 8) 2)))
  (is (= [1 8 27 64] (map (c/pwr 3) [1 2 3 4])))
  (is (= [1 2 4 8 16] (map #((c/pwr %) 2) [0 1 2 3 4]))))

(deftest cartesian
  (is (= 300 (count
               (c/cartesian 
                (into #{} (range 10))
                (into #{} (range 30)))))))

(deftest symmetric-difference
  (is (= #{2 4 6 7} (c/symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7})))
  (is (= #{:a :b :c} (c/symmetric-difference #{:a :b :c} #{})))
  (is (= #{4 5 6} (c/symmetric-difference #{} #{4 5 6}))))
