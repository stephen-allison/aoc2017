(ns aoc2107.day10)
(require '[clojure.string :as str])

(def puzzle-input "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205")
(def input-lengths (map #(Integer/parseInt %) (str/split puzzle-input #",")))
(def part-2-addendum [17 31 73 47 23])
(def part-2-lengths (concat (map byte puzzle-input) part-2-addendum))

(def buffer-size 256)
(def buffer-indices (range buffer-size))
(def buffer (zipmap buffer-indices buffer-indices))

(defn select [start length] (take length (drop start (cycle buffer-indices))))

(defn step [[buf pos step] length]
  (let [keys (select pos length)
        values (for [k keys] (buf k))
        change (zipmap keys (reverse values))]
    [(merge buf change) (mod (+ pos length step) buffer-size) (inc step)]))

(defn knot-hash [data rounds] 
  (let [final-buf (first (reduce step [buffer 0 0] (flatten (repeat rounds data))))]
    (for [n buffer-indices] (final-buf n))))

(defn day10i []
  (let [final-hash (knot-hash input-lengths 1)]
    (println "day 10a " (apply * (take 2 final-hash)))))

(defn day10ii []
  (let [hash-values (knot-hash part-2-lengths 64)
        partitions (partition 16 hash-values)
        xored (map #(apply bit-xor %) partitions)
        hex (map #(format "%02X" %) xored)]
    (println "day 10b "(str/lower-case (str/join hex)))))

(defn day10 []
  (day10i)
  (day10ii))
