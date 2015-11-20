(ns bohm.math
  (:require
    [clojure.core :as core]
    [clatrix.core :as cl]))

(defn map-index
  [f col]
  (map f col (range)))

(defn ->bra
  [x1 & xis]
  (->> xis
      (concat [x1])
      (mapv identity)
      (vector)))

(defn ->ket
  [x1 & xis]
  (->> xis
       (concat [x1])
       (mapv vector)))

(defn *-dispatch
  [& xis]
  (cond
    (empty? xis)         :identity
    (= (count xis) 1)    :single
    (every? vector? xis) :matrix
    (every? number? xis) :number
    :else                :mix))

(defmulti * *-dispatch)

(defmethod * :identity
  []
  1)

(defmethod * :single
  [x]
  x)

(defmethod * :matrix
  ([x y]
   ())
  ([x y & zs]
   ()))

(defn dim-count
  [m dim]
  (loop [dim-rows m
         index    0]
    (let [termination? (= index dim)
          rows-seq?    (sequential? dim-rows)]
      (if (= index dim)
        (if rows-seq? (count dim-rows) 0)
        (if-not rows-seq? 0 (recur
                              (first dim-rows)
                              (inc index)))))))

(defn transpose
  [x]
  )

(defmethod * :number
  ([x y]
   (core/* x y))
  ([x y & zs]
   (apply core/* (concat [x] [y] zs))))

