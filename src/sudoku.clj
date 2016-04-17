(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
  (set (nth (apply map vector board) y))))

(defn coord-pairs [coords]
  (for [fst coords
        snd coords]
        [fst snd]))

(defn corner-coord [coord]
  (let [[x y] coord
        corner #(* 3 (quot % 3))]
          [(corner x) (corner y)]))

(defn block-values [board coord]
  (let [[x y] (corner-coord coord)
        range-x (range x (+ 3 x))
        range-y (range y (+ 3 y))]
        (set (for [fst range-x
                   snd range-y]
                    (value-at board [fst snd])))))

(def valid-values (set (range 1 10)))

(defn valid-values-for [board coord]
  (let [taken-block (block-values board coord)
        taken-row (row-values board coord)
        taken-col (col-values board coord)
        taken (set/union taken-block taken-row taken-col)]
        (if (has-value? board coord) #{}
            (set/difference valid-values taken))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(def zero-to-nine (range 0 9))

(defn rows [board]
  (for [x zero-to-nine]
    (row-values board [x nil])))

(defn valid-one-to-nine? [value-set]
  (empty? (set/difference valid-values value-set)))

(defn valid-all? [f]
  (not (some false?
        (for [x zero-to-nine]
          (valid-one-to-nine? (f x))))))

(defn valid-rows? [board]
  (valid-all? (fn [x] (row-values board [x nil]))))

(defn cols [board]
  (for [y zero-to-nine]
    (col-values board [nil y])))

(defn valid-cols? [board]
  (valid-all? (fn [x] (col-values board [nil x]))))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
        (block-values board [x y])))

(defn valid-blocks? [board]
  (not (some false? (map valid-one-to-nine? (blocks board)))))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(def all-cords (coord-pairs zero-to-nine))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) all-cords)))

(defn solve [board]
  nil)
