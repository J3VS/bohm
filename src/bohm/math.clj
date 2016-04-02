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

(defn rows
  [x]
  (count x))

(defn columns
  [x]
  (let [first-col (count (first x))]
    (when (every? #(= first-col (count %)) x)
      first-col)))

(defn mmult-entry
  [a b row-index column-index]
  (->> a
       columns
       range
       (mapv #(* (get-in a [row-index %])
                (get-in b [% column-index])))
       (reduce +)))

(defn matrix-skeleton
  [func i j]
  (mapv (fn [ix]
          (mapv (fn [jx]
                  (func ix jx)) (range j))) (range i)))

(defn mmult
  [x y]
  (matrix-skeleton (partial mmult-entry x y) (rows x) (columns y)))

(defn equal-size
  [x y]
  (and (= (rows x) (rows y))
             (= (columns x) (columns y))))

(defn madd
  [x y]
  (when (equal-size x y)
    (matrix-skeleton (fn [i j]
                       (+ (get-in x [i j]) (get-in y [i j]))) (rows x) (columns x))))

(defn msubtract
  [x y]
  (when (equal-size x y)
    (matrix-skeleton (fn [i j]
                       (- (get-in x [i j]) (get-in y [i j]))) (rows x) (columns x))))


(defn transpose
  [x]
  (if-let [cols (columns x)]
    (matrix-skeleton (fn [i j]
                       (get-in x [j i])) cols (rows x))))


(defn square?
  [x]
  (= (rows x) (columns x)))

(defn determinant
  [x]
  (let [row-count (rows x)
        col-count (columns x)]))
