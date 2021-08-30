(ns aoc2107.day3)

(defn up [[x y]] [x (inc y)])

(defn left [[x y]] [(dec x) y])

(defn down [[x y]] [x (dec y)])

(defn right [[x y]] [(inc x) y])

(defn spiral-path-moves []
  (let [square-sides (for [n (iterate inc 1)] (inc (* n 2)))
        steps-for-square (fn [sq] [right 
                                   (repeat (- sq 2) up) 
                                   (repeat (dec sq) left)
                                   (repeat (dec sq) down)
                                   (repeat (dec sq) right)])]
    (flatten (for [s square-sides] (steps-for-square s)))))

(defn move [pos move-fn]
  (move-fn pos))

(defn spiral-path []
  (reductions move [0 0] (spiral-path-moves)))

(defn day3-1 [end-square]
  (let [[end-x end-y] (nth (spiral-path) (dec end-square))]
    (println "day 3a " (+ (Math/abs end-x) (Math/abs end-y)))))

(def day3-input 325489)

(def up-right (comp up right))

(def up-left (comp up left))

(def down-right (comp down right))

(def down-left (comp down left))

(def directions [right up-right up up-left left down-left down down-right])

(defn neighbours [pos] (map #(% pos) directions))

(defn cell-value [cells pos]
  (let [adjacent (neighbours pos)
        vals (map #(get cells % 0) adjacent)]
    (if (= [0 0] pos) 1
        (apply + vals))))

(defn cell-value-and-max [[cells max-val] pos]
  (let [new-val (cell-value cells pos)]
    [(assoc cells pos new-val) (max max-val new-val)]))

(defn day3-2 [target]
  (let [values (reductions cell-value-and-max [{} 0] (spiral-path))
        [_ v] (first (drop-while (fn [[_ v]] (< v target)) values))]
    (println "day 3b " v)))

(defn day3 []
  (day3-1 day3-input)
  (day3-2 day3-input))
