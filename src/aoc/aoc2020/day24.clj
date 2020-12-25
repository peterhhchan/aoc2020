(ns aoc.aoc2020.day24
  (:require [clojure.string :as s]))

(defn tile [t]
  (let [f     (frequencies t)
        ew    (- (get f "e" 0) (get f "w" 0))
        ns    (- (+ (get f "nw" 0) (get f "ne" 0))
                 (+ (get f "sw" 0) (get f "se" 0)))
        ew-ns (- (+ (get f "ne" 0) (get f "se" 0))
                 (+ (get f "nw" 0) (get f "sw" 0)))]
    [(+ ew (/ ew-ns 2)), ns]))

(defn tiles []
  (->> (slurp "data/aoc2020_day24.txt")
       s/split-lines
       (map #(->> (re-seq #"(se|sw|ne|nw|e|w)" %)
                  (map first)))
       (map tile)))

(defn black? [f] (odd? (second f)))

(defn part-1 []
  (->> (tiles)
       (frequencies)
       (filter black?)
       count))

(defn neighbors [pos]
  (->> ["ne" "nw" "se" "sw" "e" "w"]
       (map (comp tile list))
       (map #(mapv + % pos))))

(defn step [blacks]
  (->> (mapcat neighbors blacks)
       (frequencies)
       (filter (fn [[t cnt]]
                 (or  (#{2} cnt)
                      (and (blacks t)
                           (#{1 2} cnt)))))
       (map first)
       set))

(defn part-2 []
  (->> (tiles)
       (frequencies)
       (filter black?)
       (map first)
       set
       (iterate step)
       (drop 100)
       first
       count))
