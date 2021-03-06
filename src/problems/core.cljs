(ns problems.core)

;replicate a sequence
(defn replicator [coll n]
  (reduce (fn [result item]
            (concat result (repeat n item))) [] coll))

;fibonacci
(defn fib [n]
  (if (<= n 1)
    (list n)
    (loop [start 2
           end n
           acc [0 1]]
      (let [last-idx (dec (count acc))
            second-to-last-idx (dec last-idx)
            sum (+ (nth acc last-idx) (nth acc second-to-last-idx))]
        (if (= start end)
          (rest (conj acc sum))
          (recur (inc start) end (conj acc sum)))))))

;factorial
(defn factorial [n]
  (loop [current n
         acc n]
    (if (= current 1)
      acc
      (let [next (dec current)]
        (recur next (* acc next))))))

;factorial with reduce
(defn fact-reduce [n]
  (reduce * (range 1 (inc n))))

;a half-truth
(defn half-truth
  "Takes a variable number of booleans and returns true if some parameters are true but not all"
  [& bs]
  (cond
    (every? #(true? %) bs) false
    (every? #(false? %) bs) false
    :else (some #(true? %) bs)))

;greatest common divisor
(defn gcd
  "Given two integers, return the greatest common divisor"
  [x y]
  (let [minimum (min x y)
        maximum (max x y)
        candidates (->> minimum inc (range 1) reverse)
        divisor? (fn [n divisor] (= 0 (rem n divisor)))
        min-divisors (filter #(divisor? minimum %) candidates)
        max-divisors (filter #(divisor? maximum %) min-divisors)]
    (first max-divisors)))

;simple closures
(defn pwr
  "Given a positive integer, return a function f(x) that calculates x^n"
  [n]
  (fn [x]
    (Math/pow x n)))

;cartesian product
(defn cartesian
  "Calculates cartesian product A x B of two sets A, B"
  [a b]
  (loop [base a
         iterating b
         result []]
    (if (empty? base)
      (set result)
      (let [current (map (fn [v] [(first base) v]) iterating)]
        (recur (rest base) iterating (concat result current))))))

;symmetric difference
(defn symmetric-difference
  "Calculate the symmetric difference of two sets"
  [a b]
  (let [intersection (clojure.set/intersection a b)]
    (clojure.set/union 
      (clojure.set/difference a intersection)
      (clojure.set/difference b intersection))))

(defn euclid-gcd
  "Calculate the greatest common divisor of two positive integers"
  [x y]
  (let [minimum (min x y)
        maximum (max x y)
        remainder (rem maximum minimum)]
    (if (= 0 remainder)
      minimum
      (recur minimum remainder))))

(defn lcm
  "Calculate the least common multiple of given integers, ratios"
  [x y & xs]
  (let [numbers (->> (vector x y xs)
                     flatten
                     (filter some?))
        gcd (fn [a b] 
              (let [minimum (min a b)
                    maximum (max a b)
                    remainder (rem maximum minimum)]
                (if (= 0 remainder)
                  minimum
                  (recur minimum remainder))))]
    (/ (reduce * numbers) (reduce gcd numbers))))

;pascal's triangle
(defn pascal
  "Returns the nth row of Pascal's Triangle"
  [n]
  (letfn [(pascal-number [row index]
            (cond 
              (= 1 row) 1
              (zero? index) 1
              (= index (dec row)) 1
              :else (+ (pascal-number (dec row) (dec index)) 
                       (pascal-number (dec row) index))))]
    (for [index (range n)]
      (pascal-number n index))))

;to tree or not to tree
(defn tree?
  "Returns true if a given seq is a binary tree"
  [t]
  (letfn [(check-tree [tree] 
            (if (or
                  (and 
                    (sequential? tree)
                    (seq tree))
                  (nil? tree))
              (let [head (first tree)
                    left (first (rest tree))
                    right (second (rest tree))
                    qt (count tree)]
                (if (not (nil? head))
                  (and (= 3 qt) (check-tree left) (check-tree right))
                  true))
              false))]
    (check-tree t)))

;beauty is symmetry
(defn symmetric-tree?
  "Returns true if a given binary tree is symmetric"
  [tree]
  (letfn [(left [t]
            (first (rest t)))
          (right [t]
            (second (rest t)))
          (mirrored? [l r] 
            (if
              (and
                (nil? l)
                (nil? r))
              true
              (let [lvalue (first l)
                    rvalue (first r)]
                (if (not (= lvalue rvalue))
                  false
                  (and
                    (mirrored? (left l) (right r))
                    (mirrored? (right l) (left r)))))))]
    (mirrored?
      (left tree)
      (right tree))))

;flipping out
(defn flip
  "Return a function g which flips the order of the arguments of an input function f"
  [f]
  (fn [a b]
    (f b a)))

;rotate a sequence
(defn rotate
  "Rotates a given sequence left or right by quantity"
  [quantity coll]
  (letfn [(rotate-left [elems n]
            (if (= 0 n)
              elems
              (let [elem (first elems)
                    remaining (rest elems)
                    updated (concat remaining [elem])]
                (recur updated (dec n)))))
          (rotate-right [elems n]
            (if (= 0 n)
              elems
              (let [elem (last elems)
                    remaining (butlast elems)
                    updated (concat [elem] remaining)]
               (recur updated (dec n)))))]
    (if (> quantity 0)
      (rotate-left coll quantity)
      (rotate-right coll (Math/abs quantity)))))

;reverse interleave
(defn reverse-interleave
  "Reverses the interleave process into n number of subsequences"
  [coll n]
  (loop [acc (vec (repeat n []))
         current 0
         elems coll]
    (if (empty? elems)
      acc
      (let [elem (first elems)
            subsequence (conj (nth acc current) elem)
            next-index (if (-> current
                               inc
                               (>= n))
                         0
                         (inc current))]
        (recur (assoc acc current subsequence)
               next-index
               (rest elems))))))

;split by type
(defn split-by-type
  "Split a seq of different types into homogeneous sub-sequences"
  [coll]
  (-> (group-by type coll)
      vals))

;prime numbers
(defn primes
  "Returns the first n number of primes"
  [n]
  (letfn [(divisable? [a b]
            (= 0 (rem a b)))
          (prime? [n]
            (if (< n 2)
              false
              (let [trials (range 2 n)]
                (not-any? #(divisable? n %) trials))))]
    (let [candidates (rest (range))
          primes (for [x candidates :when (prime? x)]
                   x)]
      (take n primes))))

;anagram finder
(defn anagrams
  "Given a vector of words return a set of sets containing word anagrams"
  [words]
  (letfn [(normalize [word]
            (clojure.string/join (sort word)))]
    (let [normalized (map #(normalize %) words)
          freqs (frequencies normalized)
          anagram-freqs (filter #(> (val %) 1) freqs)
          anagrams (reduce (fn [result item]
                             (assoc result (key item) #{})) {} anagram-freqs)]
      (into #{} (vals
                  (reduce (fn [result word]
                            (let [key (normalize word)]
                              (if (result key)
                                (assoc result key (conj (result key) word))
                                result))) anagrams words))))))
