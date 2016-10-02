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

(defn mvector?
  [x]
  (= (columns x) 1))

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
    (-> x
        cl/matrix
        cl/det)))

(defn clatrix->vv
  [cla]
  (matrix-skeleton (fn [i j]
                     (cl/get cla i j)) (cl/nrows cla) (cl/ncols cla)))

(defn inverse
  [x]
  (-> x
      cl/matrix
      cl/i
      clatrix->vv))

(defn inner-product
  [x y]
  (when (and (mvector? x)
             (mvector? y)
             (= (rows x) (rows y)))
    (matrix-skeleton (fn [i j]
                       (* (mval x i j)
                          (mval y i j))) (rows x) 1)))

(defn eigen
  [m]
  (let [{:keys [values vectors]} (-> m
                                     cl/matrix
                                     cl/eigen)]
    (map-indexed (fn [ind v]
                   {:value v
                    :vector (->> vectors
                                 (mapv #(nth % ind))
                                 cl/matrix)}) values)))
