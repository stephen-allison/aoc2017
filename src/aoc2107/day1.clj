(ns aoc2107.day1)
(require '[util.loader :as loader])
(require '[clojure.string :as str])


(defn get-input [] (loader/load "day_01_input.txt"))

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


