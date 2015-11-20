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

(defn ->dimensions
  [m]
  (loop [dim-rows m
         acc      []]
    (if-not (sequential? dim-rows)
      acc
      (recur
        (first dim-rows)
        (conj acc (count dim-rows))))))

(defn transpose
  [x]
  )

(defmethod * :number
  ([x y]
   (core/* x y))
  ([x y & zs]
   (apply core/* (concat [x] [y] zs))))

