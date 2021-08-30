(ns aoc2107.day6)

; day 6

(def test-allocation [0 2 7 0])

(def part-one-allocation [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])

(defn allocation->map [alloc]
  (into (sorted-map) (zipmap (iterate inc 0) alloc)))

(defn fullest-block [alloc-map]
  (apply max-key val (reverse alloc-map)))

(defn reallocate [alloc-map start value]
  (let [indexes (take value (drop (inc start) (cycle (range (count alloc-map)))))
        new-alloc (assoc alloc-map start 0)]
    (reduce #(update %1 %2 inc) new-alloc indexes)))

(defn alloc-str [alloc]
  (into [] (for [k (range (count alloc))] (alloc k))))

(defn reallocate-until-repeat [alloc-map seen-update]
  (loop [seen #{}
         alloc alloc-map
         n 0]
    (let [[index size] (fullest-block alloc)
          new-alloc (reallocate alloc index size)
          already-seen (contains? seen new-alloc)]
      (if (true? already-seen)
        [(inc n) (alloc-str new-alloc)]
        (recur (seen-update seen new-alloc) new-alloc (inc n))))))

(defn day6 []
      (let [[n1 final-alloc-1] (reallocate-until-repeat (allocation->map part-one-allocation) conj)
            next-map (allocation->map final-alloc-1)
            update (fn [_ _] #{next-map})
            [n2 final-alloc-2] (reallocate-until-repeat next-map update)]
        (println "day 6a " n1)
        (println "day 6b " n2)))
