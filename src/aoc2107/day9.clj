(ns aoc2107.day9)
(require '[util.loader :as loader])

(defn start-state [] {:skip false :group-depth 0 :state :group :group-scores [] :garbage-count 0})
(defn inc-depth [state] (update state :group-depth inc))
(defn score-group [state] (update state :group-scores #(conj % (:group-depth state))))
(defn dec-depth [state] (update state :group-depth dec))
(defn state->group [state] (assoc state :state :group))
(defn state->garbage [state] (assoc state :state :garbage))
(defn skip [state] (assoc state :skip true))
(defn skipped [state] (assoc state :skip false))
(defn count-garbage [state] (update state :garbage-count inc))

(def start-group (comp state->group inc-depth))
(def end-group (comp dec-depth score-group))

(def handlers {
               :group {\{ start-group
                       \} end-group
                       \< state->garbage
                       \! skip}
               :garbage {\> state->group
                         \! skip}
               })

(defn get-handler [state c]
  (if (= state :garbage)
    (get-in handlers [state c] count-garbage)
    (get-in handlers [state c] identity)))

(defn handle-char [state c]
  (let [cur-state (:state state)
        skip (:skip state)]
    (if (true? skip)
      (skipped state)
      ((get-handler cur-state c) state))
    ))

(defn day9 []
  (let [data (loader/load "day_09_input.txt")
        result (reduce handle-char (start-state) data)]
    (println "day 9a " (reduce + (:group-scores result)))
    (println "day 9b " (:garbage-count result))))
