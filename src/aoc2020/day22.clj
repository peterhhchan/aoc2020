(ns aoc2020.day22
  (:require [clojure.string :as s]))


(defn toInt [s]
  (Integer/parseInt s))

(defn data []
  (let [players (->> (s/split (slurp "data/aoc2020_day22.txt") #"\n\n")
                     (map (fn [lines]
                            (->> (s/split-lines lines)
                                 (drop 1)
                                 (mapv toInt)))))]
 {:p1 (first players)
  :p2 (second players)}))

(defn toLong [s]
  (Long/parseLong s))

(defn play-game [d1 d2]
  (loop [[f1 & r1] d1
         [f2 & r2] d2]
    (if (and f1 f2)
      (if (> f1 f2)
        (recur (concat r1 [f1 f2]) r2)
        (recur r1 (concat r2 [f2 f1])))
      (if f1
        [:p1 (concat [f1] r1) ]
        [:p2 (concat [f2] r2) ]))))

(defn part-1 []
  (let [d (data)]
    (play-game (:p1 d) (:p2 d))))

(def m-mem
  (memoize
   (fn [player1 player2 ]
     (loop [d1 (vec player1)
            d2 (vec player2)
            games-played #{}]
       (cond
             (empty? d2)
             [:p1 d1]
             (empty? d1)
             [:p2 d2]
             (games-played [d1 d2])
             [:p1 d1]
             :else
             (let [[f1 & r1] d1
                   [f2 & r2] d2
                   sub-game? (and (<= f1 (count r1)) (<= f2 (count r2)))]
               (if (or (and sub-game? (= :p1 (first (m-mem (take f1 r1) (take f2 r2)))))
                       (and (not sub-game?) (> f1 f2)))
                   (recur (concat r1 [f1 f2]) r2 (conj games-played [(vec d1) (vec d2)]))
                   (recur r1 (concat r2 [f2 f1]) (conj games-played [(vec d1) (vec d2)])))))))))


(defn part-2 []
  (let [d (data)
        winner    (m-mem (:p1 d) (:p2 d))
        deck-size (count (second winner))]
    (->> (map #(vector %1 %2) (second winner) (reverse (range 1 (inc deck-size))))
         (map (partial apply *))
         (reduce +))))
