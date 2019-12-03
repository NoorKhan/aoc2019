(ns aoc2019.core
  (:require [clojure.java.io :as io]))

(def input-file (io/resource "input1"))

(def numbers (clojure.string/split-lines (slurp input-file)))

(defn fuel-required
  [mass]
  (- (quot mass 3) 2))

(fuel-required 1969)
(fuel-required 100756)

(apply + (map #(fuel-required (Integer/parseInt %)) numbers))

(defn zero-or-negative? [n]
  (or (= n 0) (< n 0)))

(zero-or-negative? -55)

(defn real-fuel-required
  [mass]
  (loop [current-fuel-required (fuel-required mass)
         total 0]
    (if (zero-or-negative? current-fuel-required)
      total
      (recur (fuel-required current-fuel-required) (+ total current-fuel-required)))))

(real-fuel-required 1969)
(real-fuel-required 100756)

(apply + (map #(real-fuel-required (Integer/parseInt %)) numbers))
