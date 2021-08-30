(ns aoc2107.day4)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

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
