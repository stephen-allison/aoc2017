(ns aoc2107.day10)

(def input [88 88 211 106 141 1 78 254 2 111 77 255 90 0 54 205])

(def numbers (iterate inc 0))

(defn buffer [n] (zipmap (take n numbers) (take n numbers)))

(defn select [start n size]
  (take n (drop start (cycle (range size)))))

(defn step [[buf pos step] length]
  (let [size (count buf)
        keys (select pos length size)
        values (for [k keys] (buf k))
        change (zipmap keys (reverse values))]
    [(merge buf change) (mod (+ pos length step) size) (inc step)]))

(defn day10 []
  (let [buf (buffer 256)
        final-buf (first (reduce step [buf 0 0] input))]
    (println "day 10a " (* (final-buf 0) (final-buf 1)))))

(day10)



