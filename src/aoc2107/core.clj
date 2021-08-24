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
    (do (println "day 1a " 
                 (day1-1 numbers))
        (println "day 1b "
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

(def day3-input 325489)

(day3-1 day3-input)

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

(day3-2 day3-input)


; day 4
(defn words [line] (str/split line #" "))

(defn get-day4-input [] (map words (str/split-lines (loader/load "day_04_input.txt"))))

(defn unique-count [coll] (count (into #{} coll)))

(defn valid-phrase? [phrase] (= (count phrase) (unique-count phrase)))

(defn inc-or-one [n]
  (if (nil? n) 
    1
    (inc n)))

(defn counts [coll]
  (reduce #(update-in %1 [%2] inc-or-one) {} coll))

(defn has-anagram? [words]
  (let [letter-counts (map counts words)
        count-counts (counts letter-counts)]
    (> (apply max (vals count-counts)) 1)))

(defn day4[]
  (let [valid-1 (filter valid-phrase? (get-day4-input))
        valid-2 (filter (complement has-anagram?) valid-1 )]
    (println "day 4a " (count valid-1))
    (println "day 4b " (count valid-2))))

(day4)


; day 5 - part b is a bit slow, array might be a better choice than a map

(def test-jumps [0 3 0 1 -3])

(defn get-day5-input []
  (map #(Integer/parseInt %) (str/split-lines (loader/load "day_05_input.txt"))))

(defn jumps->map [jumps]
  (zipmap (iterate inc 0) jumps))

(defn jump-around 
  ([jump-map inc-fn] (jump-around jump-map 0 0 inc-fn))
  ([jump-map pos n inc-fn]
   (if-let [distance (get jump-map pos)]
     (recur(update-in jump-map [pos] inc-fn) (+ pos distance) (inc n) inc-fn)
     n)))

(defn crazy-inc [n]
  (if (>= n 3)
    (dec n)
    (inc n)))

(defn day5 []
  (let [input-data (get-day5-input)
        jump-map (jumps->map input-data)]
    (println "day 5a " (jump-around jump-map inc))
    (println "day 5b " (jump-around jump-map crazy-inc))))

(day5)
