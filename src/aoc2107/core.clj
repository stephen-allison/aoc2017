(ns aoc2107.core)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

(defn get-input [] (loader/load "day_01_input.txt"))

; day 1

(defn get-input-numbers [] 
  (map #(Integer/parseInt %) (str/split (str/trim-newline (get-input)) #"")))

(defn rotate 
  ([v] (rotate v 1))
  ([v n] (concat (drop n v) (take n v))) )

(defn calculate [vals rotated]
  (let [numbers
        (map #(if (= %1 %2) %1 0) vals rotated)]
    (apply + numbers)))

(defn day1-1 [numbers]
  (calculate numbers (rotate numbers)))

(defn day1-2 [numbers]
  (let [rotation (/ (count numbers) 2)]
    (calculate numbers (rotate numbers rotation))))

(defn day1 []
  (let [numbers (get-input-numbers)]
    (do (println "part 1 " 
                 (day1-1 numbers))
        (println "part 2 "
                 (day1-2 numbers)))))

(day1)

; day 2

(defn get-checksum-numbers []
  (loader/load "day_02_input.txt"))

(defn make-row-numeric [strs]
  (map #(Integer/parseInt %) strs))

(defn rows [] 
  (map make-row-numeric 
       (map #(str/split % #"\t") 
            (str/split-lines 
             (get-checksum-numbers)))))

(defn row-range [row]
  (- (apply max row) (apply min row)))

(defn day2-1 []
  (println "day 2a " (apply + (map row-range (rows)))))

(day2-1)

(defn pairs [row]
  (for [x (range (count row)) y (range (count row)) 
        :when (< x y)] [(nth row x) (nth row y)]))

(defn divide-pair [pair]
  (/ (apply max pair) (apply min pair)))

(defn divisible? [pair]
  (= 0 (mod (apply max pair) (apply min pair))))

(defn divisible-pair [row]
  (first (filter divisible? (pairs row))))

(defn day2-2 []
  (println "day 2b " (apply + (map divide-pair (map divisible-pair (rows))))))

(day2-2)

; day 3

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

(nth (spiral-path) 6)

(day3-1 325489)

(nth (reductions move [0 0] (spiral-path)) 2)
