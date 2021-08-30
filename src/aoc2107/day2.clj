(ns aoc2107.day2)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

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

