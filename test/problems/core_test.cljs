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

(deftest euclid-gcd
  (is (= 2 (c/euclid-gcd 2 4)))
  (is (= 5 (c/euclid-gcd 10 5)))
  (is (= 1 (c/euclid-gcd 5 7)))
  (is (= 33 (c/euclid-gcd 1023 858))))

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
  (is (= #{2 4 6 7} 
         (c/symmetric-difference 
           #{1 2 3 4 5 6} 
           #{1 3 5 7})))
  (is (= #{:a :b :c} (c/symmetric-difference #{:a :b :c} #{})))
  (is (= #{4 5 6} (c/symmetric-difference #{} #{4 5 6})))
  (is (= #{[1 2] [3 4]} 
         (c/symmetric-difference
           #{[1 2] [2 3]}
           #{[2 3] [3 4]}))))

(deftest lcm
  (is (= 6 (c/lcm 2 3)))
  (is (= 105 (c/lcm 5 3 7))))

(deftest pascal
  (is (= [1] (c/pascal 1)))
  (is (= [1 10 45 120 210 252 210 120 45 10 1]
         (c/pascal 11))))

(deftest tree?
  (is (= false
         (c/tree? '(:a nil ()))))
  (is (= true
         (c/tree? '(:a (:b nil nil) nil))))
  (is (= false
         (c/tree? '(:a (:b nil nil)))))
  (is (= true 
         (c/tree? [1 nil [2 [3 nil nil] [4 nil nil]]])))
  (is (= false
         (c/tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])))
  (is (= true
         (c/tree? [1 [2 [3 [4 nil nil] nil] nil] nil])))
  (is (= false
         (c/tree? [1 [2 [3 [4 false nil] nil] nil] nil]))))

(deftest sym-tree?
  (is (= true
         (c/symmetric-tree? '(:a (:b nil nil) (:b nil nil)))))
  (is (= false
         (c/symmetric-tree? '(:a (:b nil nil) nil))))
  (is (= false
         (c/symmetric-tree? '(:a (:b nil nil) (:c nil nil)))))
  (is (= true
         (c/symmetric-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]))))

(deftest flip
  (is (= 3
         ((c/flip nth) 2 [1 2 3 4 5])))
  (is (= true
         ((c/flip >) 7 8)))
  (is (= 4
         ((c/flip quot) 2 8)))
  (is (= [1 2 3]
         ((c/flip take) [1 2 3 4 5] 3))))

(deftest rotate
  (is (= '(3 4 5 1 2)
         (c/rotate 2 [1 2 3 4 5])))
  (is (= '(4 5 1 2 3)
         (c/rotate -2 [1 2 3 4 5])))
  (is (= '(2 3 4 5 1)
         (c/rotate 6 [1 2 3 4 5])))
  (is (= '(:b :c :a)
         (c/rotate 1 '(:a :b :c))))
  (is (= '(:c :a :b)
         (c/rotate -4 '(:a :b :c)))))

(deftest reverse-interleave
  (is (= '((1 3 5) (2 4 6))
         (c/reverse-interleave [1 2 3 4 5 6] 2)))
  (is (= '((0 3 6) (1 4 7) (2 5 8))
         (c/reverse-interleave (range 9) 3)))
  (is (= '((0 5) (1 6) (2 7) (3 8) (4 9)
           (c/reverse-interleave (range 10) 5)))))

(deftest split
  (is (= #{[1 2 3] [:a :b :c]}
         (set (c/split-by-type [1 :a 2 :b 3 :c]))))
  (is (= #{[:a :b] ["foo" "bar"]}
         (set (c/split-by-type [:a "foo" "bar" :b]))))
  (is (= #{[[1 2] [3 4]] [:a :b] [5 6]}
         (set (c/split-by-type [[1 2] :a [3 4] 5 6 :b])))))

(deftest primes
  (is (= [2 3]
         (c/primes 2)))
  (is (= [2 3 5 7 11]
         (c/primes 5)))
  (is (= 541
         (last (c/primes 100)))))

(deftest anagrams
  (is (= #{#{"meat" "team" "mate"}}
         (c/anagrams ["meat" "mat" "team" "mate" "eat"])))
  (is (= #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}
         (c/anagrams ["veer" "lake" "item" "kale" "mite" "ever"]))))
