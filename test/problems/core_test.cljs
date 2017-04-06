(ns problems.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [problems.core :as c]))

(deftest test-replicator
  (is (= '(1 1 2 2 3 3) (c/replicator [1 2 3] 2)))
  (is (= '(:a :a :a :a :b :b :b :b) (c/replicator [:a :b] 4)))
  (is (= '(4 5 6) (c/replicator [4 5 6] 1)))
  (is (= '([1 2] [1 2] [3 4] [3 4]) (c/replicator [[1 2] [3 4]] 2)))
  (is (= [44 44 33 33] (c/replicator [44 33] 2))))

(deftest test-fibonacci
  (is (= '(1 1 2) (c/fib 3)))
  (is (= '(1 1 2 3 5 8) (c/fib 6)))
  (is (= '(1 1 2 3 5 8 13 21) (c/fib 8))))
