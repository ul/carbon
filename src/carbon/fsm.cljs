(ns carbon.fsm
  (:require [carbon.rx :as rx :include-macros true]))

(defn- enter
  [state spec ctx event]
  (doseq [action (get-in spec [state :enter])] (action ctx event)))

(defn- exit
  [state spec ctx event]
  (doseq [action (get-in spec [state :exit])] (action ctx event)))

(defn- input
  [state spec ctx event]
  (doseq [[condition action] (get-in spec [state :input])
          :when (condition ctx event)]
    (action ctx event)))

(defn- transit
  [state spec ctx event]
  (reduce (fn [state [condition next-state]]
            (if (condition ctx event)
              (reduced (do (exit state spec ctx event)
                           (enter next-state spec ctx event)
                           (transit next-state spec ctx event)))
              state))
    state
    (get-in spec [state :transitions])))

(defn execute
  [state spec ctx event]
  (input state spec ctx event)
  (transit state spec ctx event))

(defn execute!
  [state spec ctx event]
  (rx/dosync (swap! state execute spec ctx event)))

(defn on
  "Returns a function which detects if the first element of its second argument is in `xs` set.
  This is convenient for writing input/transition conditions based on matching element type
  represented as the first vector element (if you use sum type for events)"
  [& xs]
  (let [s (set xs)] (fn [_ [y]] (contains? s y))))

(defn with [& xs] (let [s (set xs)] (fn [_ [_ y]] (contains? s y))))

(defn and-fn
  [& preds]
  (fn [& xs]
    (loop [preds (seq preds)]
      (if preds (and (apply (first preds) xs) (recur (next preds))) true))))