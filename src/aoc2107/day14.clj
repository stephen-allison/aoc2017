(ns aoc2107.day14)
(require '[clojure.string :as str])
(require '[aoc2107.day10 :as day10])

(defn pad [bin-str]
  (let [extra-needed (- 8 (count bin-str))
        padding (apply str (repeat extra-needed "0"))]
    (str padding bin-str)))

(defn make-input [base-input]
  (map #(format "%s-%s" base-input %) (range 128)))

(defn hash-to-binary [input]
  (let [bytes (concat (map byte input) part-2-addendum)
        hash-values (day10/knot-hash bytes 64)
        partitions (partition 16 hash-values)
        xored (map #(apply bit-xor %) partitions)
        bin  (map (comp pad #(Integer/toBinaryString %)) xored)]
    (apply str bin)))

(defn day14[]
  (->> (make-input "vbqugkhl")
       (map hash-to-binary)
       (flatten)
       (apply str)
       (filter #(= 1 %))
       (count)))

