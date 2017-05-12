(ns anticipation.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn satisfies-rule-1?
  [target test-against]
  (loop [a target b test-against]
    (if (and (= (first b) 1) (= (first a) 1))
      false
      (if (or (= a nil) (= b nil))
        true
        (recur (next a) (next b))))))

(defn satisfies-rule-2?
  [target test-against index]
  (if (= (nth test-against index) 1)
    (= (nth target index) 0)
    true))

(defn satisfies-rule-1-many?
  [target & test-against]
  (loop [t test-against]
    (if (not (satisfies-rule-1? target (first t)))
      false
      (if (= t nil)
        true
        (recur (next t))))))

(defn p-rule
  [target production]
  {:target target :production production})

(defn apply-p-rule
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
  [seed]
  (filter
    (fn [s] (satisfies-rule-1? s seed))
    (combo/permutations seed)))

(defn get-satisfactory-sets-for-rule-2
  [seed index]
  (filter
    (fn [s] (satisfies-rule-2? s seed index))
    (combo/permutations seed)))

(defn get-satisfactory-sets-for-both-rules
  [seed index]
  (filter
    (fn [s](and (satisfies-rule-1? s seed) (satisfies-rule-2? s seed index)))
    (combo/permutations seed)))

(defn construct-permutations-tree
  [root depth]
  (recur [level 0]))

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
