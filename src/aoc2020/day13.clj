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
  (prn bs)
  (let [first-stop
        (loop [times (map generate-times bs)]
          (let [vs (map first times)
                mn (apply min vs)
                mx (max (apply max vs) )]
            (if (= mn mx)
              mn
              (recur (->> times
                          (map (fn [ts] (drop-while #(< % mx) ts))))))))
        bus-freq (reduce * (map second bs))]
    [(- bus-freq first-stop)  bus-freq]))

(defn buses []
  (->> (s/split (second (data))  #",")
       (keep-indexed #(when (not= %2 "x") [%1 (toInt %2)]))
       (sort-by second)))

(defn part-2 []
  (loop [bs (buses)]
    (if (> (count bs) 1)
      (recur (->> (cons (replace-bus (take 2 bs)) (drop 2 bs))
                  (sort-by second )))
      (let [[stop freq] (first bs)]
        (- freq stop)))))

(defn part-2b []
  ;; Take 2
  ;; Optimal solution is to find the two factors closest to the squareroot
  (loop [bs (buses)]
    (prn (count bs))
    (if (> (count bs) 1)
      (if (= (count bs) 2)
        (recur (->> (cons (replace-bus (take 2 bs)) (drop 2 bs))
                    (sort-by second )))
        (let [b1 (first bs)
              b2 (first (drop 1 (reverse bs)))
              bss (set bs)]
          (recur (->> (cons (replace-bus [b1 b2]) (seq (disj bss b1 b2)))
                      (sort-by second )))))
      (let [[stop freq] (first bs)]
        (- freq stop)))))
