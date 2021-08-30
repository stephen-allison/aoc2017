(ns util.counter)

(defn- inc-count [counts value]
  (if (contains? counts value) 
    (update counts value inc)
    (assoc counts value 1)))

(defn count-items [items-to-count]
  (reduce #(inc-count %1 %2) {} items-to-count))

(defn least-frequent [counts-map] (key (apply min-key val counts-map)))

(defn most-frequent [counts-map] (key (apply max-key val counts-map)))
