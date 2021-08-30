(ns aoc2107.day7)
(require '[util.loader :as loader])
(require '[util.counter :as counter])
(require '[clojure.string :as str]); day 7

(defn parse-day-7 [line]
  (let [[n w & c] (re-seq  #"[a-z0-9]+" line)]
    {:name n :weight (Integer/parseInt w) :children c}))

(defn find-leaves [node-map]
  (letfn [(leaf? [{c :children}] (nil? c))]
    (into #{} (map :name (filter leaf? (vals node-map))))))

(defn populate-with-parent [node-map]
  (letfn [(child->parent [{:keys [name children]}] (zipmap children (repeat {:parent name})))]
    (merge-with merge node-map (apply merge (map child->parent (vals node-map))))))

(defn find-root [node-map]
  (letfn [(no-parent? [{p :parent}] (nil? p))]
    (first (filter no-parent? (vals node-map)))))

(defn previous-generation [node-map names] 
  (into #{} (remove nil? (map #(get-in node-map [% :parent]) names))))

(defn generations [node-map]
  (take-while (complement empty?) 
              (iterate (partial previous-generation node-map) (find-leaves node-map))))

(defn total-weight [node-map {:keys [name children weight]}]
  (if (nil? children)
    {name {:total-weight weight :balanced true :child-weights nil}}
    (let [cw (remove nil? (map #(get-in node-map [% :total-weight]) children))
          balanced (apply = cw)]
      {name {:total-weight (+ weight (apply + cw)) :balanced balanced :child-weights cw}})))
  
(defn update-with-total-weights [node-map nodes]
  (let [new-fields (apply merge (map (partial total-weight node-map) nodes))]
    (merge-with merge node-map new-fields)))

(defn select-nodes [node-map names]
  (vals (select-keys node-map names)))

(defn populate-total-weights [node-map]
  (reduce 
   (fn [nm gen] (update-with-total-weights nm (select-nodes nm gen))) 
   node-map 
   (generations node-map)))

(defn balanced? [{balanced :balanced}] balanced)

(defn lightest-node [nodes] (first (sort-by :total-weight nodes)))

(defn find-correct-weight [node-map]
  (let [unbalanced-nodes (remove balanced? (vals node-map))
        unbalanced-node (lightest-node unbalanced-nodes)
        unbalanced-weights (counter/count-items (:child-weights unbalanced-node))
        wrong-weight (counter/least-frequent unbalanced-weights)
        right-weight (counter/most-frequent unbalanced-weights)
        weight-correction (- right-weight wrong-weight)
        unbalancing-node-map (zipmap (:child-weights unbalanced-node) (:children unbalanced-node))
        unbalancing-node-name (unbalancing-node-map wrong-weight)
        unbalancing-node (node-map unbalancing-node-name)]
    (+ (:weight unbalancing-node) weight-correction)))

(defn day-7-data [] 
  (->> (loader/load "day_07_input.txt")
       (str/split-lines)
       (map parse-day-7)
       (into {} (map #(vector (:name %) %)))
       (populate-with-parent)
       (populate-total-weights)))

(defn day7 []
  (let [node-map (day-7-data)]
    (println "day 7a root node is " (:name (find-root node-map)))
    (println "day 7b correct weight is " (find-correct-weight node-map))))


