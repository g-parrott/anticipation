(ns anticipation.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn satisfies-rule-1?
  "Tests if the parameters (assumed to be sequences)
  satisfy the property: test-against_i = 1 -> target_i = 0"
  [target test-against]
  (loop [a (into [] target)  b (into [] test-against)]
    (if (and (= (first b) 1) (= (first a) 1))
      false
      (if (or (= a nil) (= b nil))
        true
        (recur (next a) (next b))))))

(defn satisfies-rule-2?
  "Tests if the parameters (assumed to be sequences)
  satisfy the property: test-against_index = 1 -> target_index = 0
  It's a less restrictive version of rule-1"
  [target test-against index]
  (if (= (nth test-against index) 1)
    (= (nth target index) 0)
    true))

(defn satisfies-rule-1-many?
  "see the function, satisfies-rule-1?
  This function tests if rule-1 is satisfied for all
  sets in test-against"
  [target & test-against]
  (loop [t test-against]
    (if (not (satisfies-rule-1? target (first t)))
      false
      (if (= t nil)
        true
        (recur (next t))))))

(defn satisfies-rule-2-many?
  "see the function, satisfies-rule-2?
  This function tests if rule-2 is satisfied for
  all sets in test against. The order of the elements
  in test-against determines the index of each test"
  [target & test-against]
  (loop [a target b test-against n 0]
        (if (not (satisfies-rule-2? a (first b) n))
          false
          (recur a (rest b) (+ n 1)))))

(defn p-rule
  "Prototype for a hash-map that represents a production
  rule in an LSystem"
  [target production]
  {:target target :production production})

(defn apply-p-rule
  "Applies a production rule (see p-rule) to a sequence"
  [s rule]
  (loop [r [] sq s]
    (if (= sq nil)
      r
      (recur
       (if (= (get rule :target) (first sq))
         (into [] (concat r (get rule :production)))
         (into [] (conj r (first sq))))
       (next sq)))))

(defn get-satisfactory-sets-for-rule-1
  "Given some initial sequence 'seed', assumed to
  be a sequence of binary digits, returns all sets
  which satisfy rule-1 from the permutations of 'seed'" [seed] (filter (fn [s] (satisfies-rule-1? seed s)) (combo/permutations seed)))
(defn get-satisfactory-sets-for-rule-2
  "Given some initial sequence 'seed', assumed to
  be a sequence of binary digits, returns all sets
  which satisfy rule-2 from the permutations of 'seed'"
  [seed index]
  (filter
    (fn [s] (satisfies-rule-2? s seed index))
    (combo/permutations seed)))

; cool
(defn make-tree-node-by-fn
  "given 'parent' and a function which takes 1 parameter
  returns a hash-map containing the parent and the result of the
  function with keys :parent and :children respectively"
  [parent child-fn]
  {:parent parent :children (child-fn parent)})

;; losing control
(defn make-children
  [parent]
  (let [children (:children  (make-tree-node-by-fn parent get-satisfactory-sets-for-rule-1))]
  (map (fn [s] (make-tree-node-by-fn s get-satisfactory-sets-for-rule-1)) children)))

; who knows?
(defn what-is-this
  [seed]
   (map (fn [m] (map make-children (:children m))) (make-children seed)))

; interesting
(flatten (what-is-this [1 0 1 0 0]))

; testing
(flatten (apply combo/cartesian-product (combo/permutations '(1 0 0)) (combo/permutations '(1 0 1))))

; who knows?
(defn idk
  [& sequences]
  (flatten (apply combo/cartesian-product (map #(combo/permutations %) sequences))))

; gettings somewhere
(idk '(1 0 0 0) '(1 1 0 0) '(1 1 1 0 1 0))

;; ugh
;;(defn but-last [s] (take (- (count s) 1) s))

;; someone probably wrote this before
;; (defn do-n [f n] (loop [i 1 v nil] (if (= i n) (f v) (recur (inc i) (f v)))))

;; more music theory stuff
;; (def western-ring (into [] (range 1 7)))
;; (defn rotate-ring-right [ring] (into [] (cons (last ring) (but-last ring))))
;; (defn rotate-ring-left [ring] (into [] (cons (last ring) (conj (rest ring) (first ring)))))

;; parse csv utils
(require '[clojure.string :as str])
(defn csv-to-vec
  "tokenizes a string which is a sequence of comma separated values"
  [string]
  (str/split string #", "))

(defn parse-nums
  ([string]
  (map
    (fn [token]
      (read-string token))
    (str/split string #" "))))

(defn parse-many-nums
  [strings]
  (map parse-nums strings))

(defn -main
  "Reads the file as a csv and outputs the resulting combinatorial sequence"
  [& args]
  (def file-string (slurp (first args)))
  (def seed (parse-many-nums (csv-to-vec file-string)))
  (def product (apply idk seed))
  (loop [i 0 v product]
    (if (empty? v)
      nil
      (do
        (if (= i 7)
          (do (print (first v))
              (print "\n"))
          (print (first v)))
        (recur (or (and (= i 7) 0) (inc i))
               ((rest v)))))))
