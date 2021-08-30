(ns util.counter)

(defn inc-count [counts value]
  (if (contains? counts value) 
    (update counts value inc)
    (assoc counts value 1)))

(defn count-items [items-to-count]
  (loop [items items-to-count
         counts {}]
    (let [value (first items)
          remaining (rest items)]
      (if (empty? remaining)
        (inc-count counts value)
        (recur remaining (inc-count counts value))))))

(defn least-frequent [counts-map] (key (apply min-key val counts-map)))

(defn most-frequent [counts-map] (key (apply max-key val counts-map)))
