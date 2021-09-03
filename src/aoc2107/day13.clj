(ns aoc2107.day13)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn puzzle-input []
  (->> (loader/load "day_13_input.txt")
       (str/split-lines)
       (map parse-line)
       (into (sorted-map))))

(defn make-scanner-map [input]
  (into (sorted-map) (map (fn [[k v]] [k (scanner-f v)])) input))

(defn scanner-f [depth]
  (fn [t] (let [q (mod t (* 2 (dec depth)))
                r (if (>= q depth) (- (- depth 2) (- q depth)) q)] 
            r )))

(def scanner-map (make-scanner-map (puzzle-input)))

(defn check-scanners [t]
  (map (fn [[tt f]] (f (+ t tt))) scanner-map))

(defn caught? [[posns _]] (some zero? posns))

(defn severity [scan-results input]
  (apply + (map #(if (zero? %2) (apply * %1) 0) input scan-results)))

(defn part-1 []
  (severity (check-scanners 0) (puzzle-input)))

; brute force, there's probably a clever chinese remander theorem approach, but tired
(defn part-2 []
  (second (first (drop-while caught? (for [t (iterate inc 0)] [(check-scanners t) t])))))

(defn day13 []
  (println "day 13a " (part-1))
  (println "day 13b " (part-2)))
