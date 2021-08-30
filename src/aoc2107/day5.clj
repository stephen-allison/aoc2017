(ns aoc2107.day5)
(require '[util.loader :as loader])
(require '[util.counter :as counter])
(require '[clojure.string :as str])


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
     (recur(update jump-map pos inc-fn) (+ pos distance) (inc n) inc-fn)
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
