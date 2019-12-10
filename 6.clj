(defn get-orbits-map [input-file]
  (reduce #(let [orbit (clojure.string/split %2 #"\)")
                 primary (first orbit)
                 satellite (second orbit)]
             (assoc %1 satellite primary))
          (sorted-map)
          (clojure.string/split-lines
           (slurp (clojure.java.io/resource input-file)))))

(def orbits-map (get-orbits-map "input6"))
orbits-map

(defn count-orbit [orbits-map satellite c]
  (let [primary (get orbits-map satellite)]
    (if (contains? orbits-map primary)
      (count-orbit orbits-map primary (inc c))
      c)))

(reduce-kv (fn [c k v] (+ c (count-orbit orbits-map k 1))) 0 orbits-map)

