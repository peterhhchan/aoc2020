(ns aoc.aoc2020.dayx
  (:require [clojure.string :as s]))

(defn data []
  (->> (slurp "data/aoc.aoc2020.day.txt")
       s/split-lines))

(defn toInt [s]
  (Integer/parseInt s)

(defn toLong [s]
  (Long/parseLong s)))

(defn part-1 []
  (let [d (data)]))

(defn part-2 []
  (let [d (data)]))
