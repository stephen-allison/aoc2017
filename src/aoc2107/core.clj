(ns aoc2107.core)
(require '[util.loader :as loader])
(require '[util.counter :as counter])
(require '[clojure.string :as str])

(defn get-input [] (loader/load "day_01_input.txt"))

; day 1

(defn get-input-numbers [] 
  (map #(Integer/parseInt %) (str/split (str/trim-newline (get-input)) #"")))

(defn rotate 
  ([v] (rotate v 1))
  ([v n] (concat (drop n v) (take n v))) )

(defn calculate [vals rotated]
  (let [numbers
        (map #(if (= %1 %2) %1 0) vals rotated)]
    (apply + numbers)))

(defn day1-1 [numbers]
  (calculate numbers (rotate numbers)))

(defn day1-2 [numbers]
  (let [rotation (/ (count numbers) 2)]
    (calculate numbers (rotate numbers rotation))))

(defn day1 []
  (let [numbers (get-input-numbers)]
    (do (println "day 1a " 
                 (day1-1 numbers))
        (println "day 1b "
                 (day1-2 numbers)))))

(day1)

; day 2

(defn get-checksum-numbers []
  (loader/load "day_02_input.txt"))

(defn make-row-numeric [strs]
  (map #(Integer/parseInt %) strs))

(defn rows [] 
  (map make-row-numeric 
       (map #(str/split % #"\t") 
            (str/split-lines 
             (get-checksum-numbers)))))

(defn row-range [row]
  (- (apply max row) (apply min row)))

(defn day2-1 []
  (println "day 2a " (apply + (map row-range (rows)))))

(day2-1)

(defn pairs [row]
  (for [x (range (count row)) y (range (count row)) 
        :when (< x y)] [(nth row x) (nth row y)]))

(defn divide-pair [pair]
  (/ (apply max pair) (apply min pair)))

(defn divisible? [pair]
  (= 0 (mod (apply max pair) (apply min pair))))

(defn divisible-pair [row]
  (first (filter divisible? (pairs row))))

(defn day2-2 []
  (println "day 2b " (apply + (map divide-pair (map divisible-pair (rows))))))

(day2-2)

; day 3

(defn up [[x y]] [x (inc y)])

(defn left [[x y]] [(dec x) y])

(defn down [[x y]] [x (dec y)])

(defn right [[x y]] [(inc x) y])

(defn spiral-path-moves []
  (let [square-sides (for [n (iterate inc 1)] (inc (* n 2)))
        steps-for-square (fn [sq] [right 
                                   (repeat (- sq 2) up) 
                                   (repeat (dec sq) left)
                                   (repeat (dec sq) down)
                                   (repeat (dec sq) right)])]
    (flatten (for [s square-sides] (steps-for-square s)))))

(defn move [pos move-fn]
  (move-fn pos))

(defn spiral-path []
  (reductions move [0 0] (spiral-path-moves)))

(defn day3-1 [end-square]
  (let [[end-x end-y] (nth (spiral-path) (dec end-square))]
    (println "day 3a " (+ (Math/abs end-x) (Math/abs end-y)))))

(def day3-input 325489)

(day3-1 day3-input)

(def up-right (comp up right))

(def up-left (comp up left))

(def down-right (comp down right))

(def down-left (comp down left))

(def directions [right up-right up up-left left down-left down down-right])

(defn neighbours [pos] (map #(% pos) directions))

(defn cell-value [cells pos]
  (let [adjacent (neighbours pos)
        vals (map #(get cells % 0) adjacent)]
    (if (= [0 0] pos) 1
        (apply + vals))))

(defn cell-value-and-max [[cells max-val] pos]
  (let [new-val (cell-value cells pos)]
    [(assoc cells pos new-val) (max max-val new-val)]))

(defn day3-2 [target]
  (let [values (reductions cell-value-and-max [{} 0] (spiral-path))
        [_ v] (first (drop-while (fn [[_ v]] (< v target)) values))]
    (println "day 3b " v)))

(day3-2 day3-input)


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

(day4)


; day 5 - part b is a bit slow, array might be a better choice than a map

(def test-jumps [0 3 0 1 -3])

(defn get-day5-input []
  (map #(Integer/parseInt %) (str/split-lines (loader/load "day_05_input.txt"))))

(defn jumps->map [jumps]
  (zipmap (iterate inc 0) jumps))

(defn jump-around 
  ([jump-map inc-fn] (jump-around jump-map 0 0 inc-fn))
  ([jump-map pos n inc-fn]
   (if-let [distance (get jump-map pos)]
     (recur(update jump-map pos inc-fn) (+ pos distance) (inc n) inc-fn)
     n)))

(defn crazy-inc [n]
  (if (>= n 3)
    (dec n)
    (inc n)))

(defn day5 []
  (let [input-data (get-day5-input)
        jump-map (jumps->map input-data)]
    (println "day 5a " (jump-around jump-map inc))
    (println "day 5b " (jump-around jump-map crazy-inc))))

;(day5)


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

(day6)


; day 7

(defn parse-day-7 [line]
  (let [[n w & c] (re-seq  #"[a-z0-9]+" line)]
    {:name n :weight (Integer/parseInt w) :children c}))

(defn day-7-data [] 
  (->> (loader/load "day_07_input.txt")
       (str/split-lines)
       (map parse-day-7)
       (into {} (map #(vector (:name %) %)))
       (populate-with-parent)
       (populate-total-weights)))

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
   (fn [nm gen] (update-with-total-weights2 nm (select-nodes nm gen))) 
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

(defn day-7 []
  (let [node-map (day-7-data)]
    (println "day 7a root node is " (:name (find-root node-map)))
    (println "day 7b correct weight is " (find-correct-weight node-map))))

(day-7)



