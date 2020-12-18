(ns aoc2020.day13
  (:require [clojure.string :as s]))

(defn data []
  (->> (slurp "data/aoc2020_day13.txt")
       s/split-lines))

(defn toInt [v]
  (Integer/parseInt v))

(defn toLong [v]
  (Long/parseLong v))

(defn part-1 []
  (let [d        (data)
        my-time  (toLong (first d))
        buses    (->> (s/split (second d) #",")
                       (remove #(= % "x"))
                       (map toInt)
                       sort)
        last-bus (map #(mod my-time %) buses)]
    (map #(- %1 %2) buses last-bus)))


(defn generate-times [[idx id]]
  (->> (- id idx)
       (iterate (partial + id))))

(defn replace-bus [bs]
  (let [first-stop
        (loop [times (map generate-times bs)]
          (let [vs (map first times)
                mn (apply min vs)
                mx (max (apply max vs) )]
            (if (= mn mx)
              mn
              (recur (->> times
                          (map (fn [ts]
                                 (drop-while #(< % mx) ts))))))))
        bus-freq (reduce * (map second bs))]
    [(- bus-freq first-stop)  bus-freq]))

(defn part-2-b []
  (let [d (data)
        bus-data (second d)
        buses    (->> (s/split bus-data  #",")
                      (keep-indexed (fn [idx busid]
                                      (when (not= busid "x")
                                        [idx (toInt busid)])))
                      (sort-by second))]
    (loop [bs buses]
      (if (> (count bs) 1)
        (recur (->> (cons (replace-bus (take 2 bs)) (drop 2 bs))
                    (sort-by second )))
        (let [[stop freq] (first bs)]
          (- freq stop))))))
