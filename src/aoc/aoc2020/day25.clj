(ns aoc2020.day25
  (:require [clojure.string :as s]))

(defn data []
  (->> (slurp "data/aoc2020_day25.txt")
       (s/split-lines)))

(defn part-1 []
  (let [d (data)])
)
