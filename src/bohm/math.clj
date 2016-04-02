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

(defn mval
  [x i j]
  (get-in x [i j]))

(defn mmult-entry
  [a b row-index column-index]
  (->> a
       columns
       range
       (mapv #(* (mval a row-index %)
                 (mval b % column-index)))
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
                       (+ (mval x i j) (mval y i j))) (rows x) (columns x))))

(defn msubtract
  [x y]
  (when (equal-size x y)
    (matrix-skeleton (fn [i j]
                       (- (mval x i j) (mval y i j))) (rows x) (columns x))))


(defn transpose
  [x]
  (if-let [cols (columns x)]
    (matrix-skeleton (fn [i j]
                       (mval x j i)) cols (rows x))))


(defn square?
  [x]
  (let [r (rows x)
        c (columns x)]
    (when (= r c)
      [r c])))

(defn remove-row
  [x r]
  (matrix-skeleton (fn [i j]
                     (if (< i r)
                       (mval x i j)
                       (mval x (+ i 1) j))) (- (rows x) 1) (columns x)))

(defn remove-column
  [x c]
  (matrix-skeleton (fn [i j]
                     (if (< j c)
                       (mval x i j)
                       (mval x i (+ j 1)))) (rows x) (- (columns x) 1)))

(defn mrest
  [x r c]
  (-> x
      (remove-row r)
      (remove-column c)))

(defn determinant
  [x]
  (if-let [[r c] (square? x)]
    (if (= r c 2)
      (- (* (mval x 0 0) (mval x 1 1))
         (* (mval x 1 0) (mval x 0 1)))
      (->> c
           range
           (map (fn [col]
                  (* (mval x 0 col) (Math/pow -1 col) (determinant (mrest x 0 col)))))
           (apply +)))))


