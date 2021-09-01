(ns aoc2107.day11)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

(def directions {:n  [1 0 -1]
                 :s  [-1 0 1]
                 :ne [0 1 -1]
                 :se [-1 1 0]
                 :nw [1 -1 0]
                 :sw [0 -1 1]})

(def steps (map keyword (str/split (str/trim (loader/load "day_11_input.txt")) #",")))

(defn add-step [a b] (mapv + a b))

(defn step [pos direction]
  (add-step pos (direction directions)))

(defn distance [[x y z]] 
  (/ (+ (Math/abs x) (Math/abs y) (Math/abs z)) 2))

(defn day11 []
  (let [distances (map distance (reductions step [0 0 0] steps))]
    (println "day 11a " (last distances))
    (println "day 11b " (apply max distances))))

(day11)
