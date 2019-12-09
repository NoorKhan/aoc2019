(defn count-valid-passwords [lower-bound upper-bound]
  (reduce #(if (is-valid-password? %2) (inc %1) %1) 0 (range lower-bound (inc upper-bound))))

(defn is-valid-password? [password]
  (let [password (str password)]
    (loop [contains-equal-adjacents? false
           i 1]
      (if (= i (count password))
        contains-equal-adjacents?
        (let [current-digit (char-to-int (get password i))
              previous-digit (char-to-int (get password (dec i)))]
          (if (> previous-digit current-digit)
            false
            (recur (or contains-equal-adjacents? (= current-digit previous-digit)) (inc i))))))))

(defn char-to-int [c]
  (-> c str (Integer/parseInt)))

(count-valid-passwords 231832 767346)

(defn is-valid-password2? [password]
  (let [password (str password)]
    (loop [adjacent-digit-count-map {}
           i 1]
      (if (= i (count password))
        (some #(= % 2) (vals adjacent-digit-count-map))
        (let [current-digit (char-to-int (get password i))
              previous-digit (char-to-int (get password (dec i)))
              equal-digits? (= current-digit previous-digit)]
          (if (> previous-digit current-digit)
            false
            (recur (if equal-digits?
                     (if (contains? adjacent-digit-count-map current-digit)
                       (merge adjacent-digit-count-map (assoc adjacent-digit-count-map current-digit (inc (get adjacent-digit-count-map current-digit))))
                       (merge adjacent-digit-count-map (assoc adjacent-digit-count-map current-digit 2)))
                     adjacent-digit-count-map)
                   (inc i))))))))

(defn count-valid-passwords2 [lower-bound upper-bound]
  (reduce #(if (is-valid-password2? %2) (inc %1) %1) 0 (range lower-bound (inc upper-bound))))

(count-valid-passwords2 231832 767346)

(is-valid-password2? 111122)
