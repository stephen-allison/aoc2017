(ns aoc2107.day12)
(require '[util.loader :as loader])
(require '[clojure.set :as set])
(require '[clojure.string :as str])


(defn add-connections 
  ([graph [node-id & connected-nodes]] (add-connections graph node-id connected-nodes))
  ([graph node-id connected-nodes] (assoc graph node-id (set connected-nodes))))

(defn build-graph [node-connections]
  (reduce #(add-connections %1 %2) {} node-connections))

(defn remove-node [graph node-id] (dissoc graph node-id))

(defn remove-nodes [graph node-ids] (reduce remove-node graph node-ids))

(defn neighbours [graph node-id] (graph node-id))

(defn traverse [graph start]
  (loop [nodes [start]
         visited []
         seen #{}]
    (if (empty? nodes)
      visited
      (let [current-node (first nodes)
            next-nodes (remove #(contains? seen %) (neighbours graph current-node))]
        (recur (concat (rest nodes) next-nodes)
               (conj visited current-node)
               (set/union seen (set (conj next-nodes current-node))))))))

(defn find-groups [graph start]
  (loop [g graph
         s start
         groups []]
    (let [visited (traverse g s)
          new-graph (remove-nodes g visited)
          new-groups (conj groups visited)]
      (if (empty? new-graph)
        new-groups
        (recur new-graph (ffirst new-graph) new-groups)))))

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

(day12)