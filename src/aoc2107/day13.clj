(ns aoc2107.day13)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

(defmulti scanner #(if (= 0 %) :empty :active))

(defmethod scanner :active [depth]
  (cycle (concat (range depth) (range (- depth 2) 0 -1))))

(defmethod scanner :empty [depth] (repeat -1))

(def fr (juxt first rest)) 

(defn wall-width [scanner-depths]
  (inc (first (apply max-key first scanner-depths))))

(defn wall-depth [scanner-depths]
  (apply max (vals scanner-depths)))

(def wall-size (juxt wall-width wall-depth))

(defn make-firewall [scanner-depths]
  (let [[width depth] (wall-size scanner-depths)]
    (map scanner (for [n (range width)] (get scanner-depths n 0)))))q

(defn scan [[now scanners]]
  (let [frs (map fr scanners)
        now (map first frs)
        later (map second frs)]
    [now later]))

(defn scan2 [[now scanners]]
  (let [[next remaining] (for [s scanners] [(first s) (rest s)])]
    [next #dbg(lazy-seq remaining)]))

(defn scanner-positions [scanners] (map first (drop 1 (iterate scan2 [[] scanners]))))
(defn scanner-positions2 [scanners] (drop 1 (iterate scan2 [[] scanners])))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn puzzle-input []
  (->> (loader/load "day_13_small_input.txt")
       (str/split-lines)
       (map parse-line)
       (into {})))

(defn run-firewall
  ([input delay] (run-firewall input delay (scanner-positions (make-firewall input))))
  ([input delay scanner-pos]
   (let [sp (drop delay scanner-pos)
         steps (wall-width input)]
     (map #(if (= 0 (nth %1 %2)) [(* %2 (input %2)) true] [0 false]) 
          (take steps sp) 
          (range steps)))))

(defn severity [result] [(apply + (map first result)) (some true? (map second result))])

(defn brute-force-firewall [input]
  (let [sp (scanner-positions (make-firewall input))]
    (map (fn [n] [(second (severity (run-firewall input n sp))) n]) (range 1000 2000))))

(severity (run-firewall (puzzle-input) 0))


(first (drop-while #(true? (first %)) (brute-force-firewall (puzzle-input))))


(first (scan2 [[] (make-firewall (puzzle-input))]))

(def sps (scanner-positions2 (make-firewall (puzzle-input))))

(first sps)

(first (rest (drop 100000 (rest (cycle (range 1201))))))

(first (drop 2000028 (cycle (concat (range 10) (range (- 10 2) 0 -1)))))

(map fr [(range 10) (range 10)])

(type (seq (cycle [1 2 3])))


(def cycles (repeat 5 (cycle (range 4))))

(defn progress-cycles [cycles]
  (let [now (map first cycles)
        later (map rest cycles)]
    (println (take 10 now))
    (println (map type later))
    later))

(def multi-cycles (map first (iterate progress-cycles cycles)))

(take 3 multi-cycles)

(map first (map rest cycles))

(map first (progress-cycles (progress-cycles cycles)))


(defn scanner-f [depth]
  (fn [t] (mod t (* 2 (dec depth)))))

(map #(%1 %2) [(scanner-f 3) (scanner-f 2) (scanner-f 4)]  (range 1 4))

(partition 10 (for  (s n) ))
