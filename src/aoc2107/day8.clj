(ns aoc2107.day8)
(require '[util.loader :as loader])
(require '[clojure.string :as str])

(defn parse-line [line]
  (let [[_ tgt tgt-op amt check check-op val] 
        (re-matches #"(\w+) (\w{3}) ([-\d]+) if (\w+) ([!=<>]*) ([-\d]+)" line)]
    [tgt tgt-op (Integer/parseInt amt) check check-op (Integer/parseInt val)]))

(defn day8-data []
  (->> (loader/load "day_08_input.txt")
       (str/split-lines)
       (map parse-line)))

(def comparisons {">" >
                  "<" <
                  "==" =
                  "!=" (comp not =)
                  ">=" >=
                  "<=" <=})

(def operations {"inc" (fn [a b] (if (nil? a) b (+ a b)))
                 "dec" (fn [a b] (if (nil? a) (- 0 b) (- a b)))})

(defn run-line [registers [tgt tgt-op amt check check-op val]]
  (let [check-value (get registers check 0)
        pass-check ((comparisons check-op) check-value val)]
    (if (true? pass-check)
      (update registers tgt #((operations tgt-op) % amt))
      registers)))

(apply max (vals (reduce run-line {} (day8-data))))
