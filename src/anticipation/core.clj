(ns anticipation.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn satisfies-rule-1?
  "Tests if the parameters (assumed to be sequences)
  satisfy the property: test-against_i = 1 -> target_i = 0"
  [target test-against]
  (loop [a target b test-against]
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
  which satisfy rule-1 from the permutations of 'seed'"
  [seed]
  (filter
    (fn [s] (satisfies-rule-1? s seed))
    (combo/permutations seed)))

(defn get-satisfactory-sets-for-rule-2
  "Given some initial sequence 'seed', assumed to
  be a sequence of binary digits, returns all sets
  which satisfy rule-2 from the permutations of 'seed'"
  [seed index]
  (filter
    (fn [s] (satisfies-rule-2? s seed index))
    (combo/permutations seed)))

(defn make-tree-node-by-fn
  "given 'parent' and a function which takes 1 parameter
  returns a hash-map containing the parent and the result of the
  function with keys :parent and :children respectively"
  [parent child-fn]
  {:parent parent :children (child-fn parent)})

(defn make-rule-1-tree
  [root depth]
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  A sequence of events is not a game design
  Don't overengineer things
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
