(ns aoc2107.day12)
(require '[util.loader :as loader])
(require '[clojure.set :as set])
(require '[clojure.string :as str])

(defn new-graph [node-ids]
  (reduce #(assoc %1 %2 #{}) {} node-ids))

(defn add-connections [graph node-id connected-nodes]
  (update graph node-id #(set/union % (set connected-nodes))))

(defn build-graph [nodes]
  (let [node-ids (map first nodes)
        graph (new-graph node-ids)]
    (reduce (fn [g [id & neighbour-ids]] (add-connections g id neighbour-ids)) graph nodes)))

(defn remove-node [graph node-id]
  (dissoc graph node-id))

(defn traverse [graph start]
  (loop [nodes [start]
         traversed []
         seen #{}]
    (if (empty? nodes)
      traversed
      (let [current-node (first nodes)
            next-nodes (remove #(contains? seen %) (graph current-node))]
        (recur (concat (rest nodes) next-nodes)
               (conj traversed current-node)
               (set/union seen (set (conj next-nodes current-node))))))))

(defn find-groups [graph start]
  (loop [g graph
         s start
         groups []]
    (let [visited (traverse g s)
          new-g (reduce #(remove-node %1 %2) g visited)
          new-groups (conj groups visited)]
      (if (empty? new-g)
        new-groups
        (recur
         new-g
         (key (first new-g))
         new-groups)))))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn puzzle-input []
  (->> (loader/load "day_12_input.txt")
       (str/split-lines)
       (map parse-line)))

(defn day12 []
  (let [graph (build-graph (puzzle-input))]
    (println "day 12a " (count (traverse graph 0)))
    (println "day 12b " (count (find-groups graph 0)))))

