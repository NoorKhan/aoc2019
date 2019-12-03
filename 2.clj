(ns advent.core
  (:require [clojure.java.io :as io]))

(def test-input "1,9,10,3,2,3,11,0,99,30,40,50")
(def numbers (vec (map #(Integer/parseInt %) (clojure.string/split test-input #","))))

(def input-file (io/resource "input2"))
(def numbers (vec (map #(Integer/parseInt %) (clojure.string/split (slurp input-file) #","))))
(def part-1-numbers (assoc (assoc numbers 1 12) 2 2))
part-1-numbers

(defn build-new-intcode-computer
  [numbers]
  (loop [i 0
         numbers numbers]
    (let [current-number (nth numbers i)]
      (cond (= current-number 99) numbers
            (= current-number 1) (recur (+ i 4) (assoc numbers (nth numbers (+ i 3)) (+ (nth numbers (nth numbers (inc i))) (nth numbers (nth numbers (+ i 2))))))
            (= current-number 2) (recur (+ i 4) (assoc numbers (nth numbers (+ i 3)) (* (nth numbers (nth numbers (inc i))) (nth numbers (nth numbers (+ i 2))))))))))

(nth (build-new-intcode-computer part-1-numbers) 0)

(defn part-2 [numbers]
  (last (for [noun (range 100)
              verb (range 100)
              :let [n (nth (build-new-intcode-computer (assoc (assoc numbers 1 noun) 2 verb)) 0)]
              :when (= 19690720 n)]
          (+ (* 100 noun) verb))))

(part-2 numbers)
