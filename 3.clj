(def test-input1 (clojure.string/split "R8,U5,L5,D3" #","))
(def test-input2 (clojure.string/split "U7,R6,D4,L4" #","))

(def input (map #(clojure.string/split % #",") (clojure.string/split-lines (slurp (clojure.java.io/resource "input3")))))
(def garret-input (map #(clojure.string/split % #",") (clojure.string/split-lines (slurp (clojure.java.io/resource "garretinput3")))))

(defn get-points
  ([path]
   (loop [x 0
          y 0
          points #{}
          i 0]
     (if (= i (count path)) points
         (let [current-step (get path i)
               current-direction (get current-step 0)
               distance (Integer/parseInt (subs current-step 1))]
           (cond
             (= current-direction \R)
             (recur
              (+ x distance)
              y
              (clojure.set/union points (get-points x y current-direction distance))
              (inc i))
             (= current-direction \U)
             (recur
              x
              (+ y distance)
              (clojure.set/union points (get-points x y current-direction distance))
              (inc i))
             (= current-direction \L)
             (recur
              (- x distance)
              y
              (clojure.set/union points (get-points x y current-direction distance))
              (inc i))
             (= current-direction \D)
             (recur
              x
              (- y distance)
              (clojure.set/union points (get-points x y current-direction distance))
              (inc i)))))))
  ([x y current-direction distance]
   (loop [x x
          y y
          points #{}
          i 0]
     (if (> i distance) points
         (cond (= current-direction \R)
               (recur (inc x) y (conj points [(inc x) y]) (inc i))
               (= current-direction \U)
               (recur x (inc y) (conj points [x (inc y)]) (inc i))
               (= current-direction \L)
               (recur (dec x) y (conj points [(dec x) y]) (inc i))
               (= current-direction \D)
               (recur x (dec y) (conj points [x (dec y)]) (inc i)))))))

(defn get-min-intersection-distance [path1 path2]
  (apply min (map #(apply + %) (reduce #(conj %1 (map (fn [n] (Math/abs n)) %2))
                                       #{}
                                       (clojure.set/intersection (get-points path1) (get-points path2))))))

(get-min-intersection-distance test-input1 test-input2) ;; 6 is correct

(def test-input3 (clojure.string/split "R75,D30,R83,U83,L12,D49,R71,U7,L72" #","))
(def test-input4 (clojure.string/split "U62,R66,U55,R34,D71,R55,D58,R83" #","))

(get-min-intersection-distance test-input3 test-input4) ;; 159 is wrong

(def test-input5 (clojure.string/split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" #","))
(def test-input6 (clojure.string/split "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #","))

(get-min-intersection-distance test-input5 test-input6) ;; 135 is correct

(def input-path1 (nth input 0))
(def input-path2 (nth input 1))

(get-min-intersection-distance input-path1 input-path2) ;; part 1
(get-min-intersection-distance (nth garret-input 0) (nth garret-input 1)) ;; garret

(defn get-intersection-points [path1 path2]
  (vec (clojure.set/intersection (get-points path1) (get-points path2))))

(def intersection-points (get-intersection-points input-path1 input-path2))
intersection-points
(get intersection-points 0)

(defn get-point-distance-map
  [path point]
  (loop [x 0
         y 0
         point-distance-map {}
         path-step 0
         current-distance-step 0
         total-distance 0]
    (if (= path-step (count path))
      point-distance-map
      (let [current-step (get path path-step)
            current-direction (get current-step 0)
            distance (Integer/parseInt (subs current-step 1))]
        (if (and (= [x y] point) (not (contains? point-distance-map point)))
          (recur x y (assoc point-distance-map point total-distance) path-step
                 current-distance-step total-distance)
          (if (= current-distance-step distance)
            (recur x y point-distance-map (inc path-step) 0 total-distance)
            (cond (= current-direction \R)
                  (recur (inc x) y point-distance-map path-step (inc current-distance-step) (inc total-distance))
                  (= current-direction \U)
                  (recur x (inc y) point-distance-map path-step (inc current-distance-step) (inc total-distance))
                  (= current-direction \L)
                  (recur (dec x) y point-distance-map path-step (inc current-distance-step) (inc total-distance))
                  (= current-direction \D)
                  (recur x (dec y) point-distance-map path-step (inc current-distance-step) (inc total-distance)))))))))

(get-point-distance-map input-path1 (get intersection-points 10))

intersection-points
(reduce #(merge %1 (get-point-distance-map input-path1 %2)) {} intersection-points)

(get-intersection-points test-input1 test-input2)

(defn get-points-distance-map [path1 points]
  (reduce #(merge %1 (get-point-distance-map path1 %2)) {} points))

(get-points-distance-map test-input1 (get-intersection-points test-input1 test-input2))
(get-points-distance-map test-input2 (get-intersection-points test-input1 test-input2))

(defn get-minimum-walked-intersection-distance [path1 path2]
  (let [intersection-points (get-intersection-points path1 path2)
        path1-points-distance-map (get-points-distance-map path1 intersection-points)
        path2-points-distance-map (get-points-distance-map path2 intersection-points)]
    (apply min (reduce
                #(conj %1 (+ (get path1-points-distance-map %2) (get path2-points-distance-map %2)))
                []
                intersection-points))))

(get-minimum-walked-intersection-distance test-input1 test-input2)
(get-minimum-walked-intersection-distance input-path1 input-path2)
