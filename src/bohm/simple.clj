(ns bohm.simple
  (:require [bohm.math :as math]))

(defn state->observation
  [state]
  (let [eig   (math/eigen state)
        total (reduce #(+ %1 (:value %2)) 0 eig)
        random-val (rand total)]
    (reduce (fn [acc {:keys [value vector]}]
              (let [new-acc (+ acc value)]
                (if (< random-val new-acc)
                  (reduced vector)
                  new-acc))) 0 eig)))
