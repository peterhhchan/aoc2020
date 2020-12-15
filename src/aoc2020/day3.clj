(ns aoc2020.day3)

(defn read-data []
  (->> (slurp "data/aoc2020_day3.txt")
       clojure.string/split-lines))

(defn part-1 []
  (let [d (read-data)
        w (count (first d))]
    (->> (range (count d))
         (map (partial * 3))
         (map #(mod % w))
         (interleave d)
         (partition 2)
         (drop 1)
         (map #(get (first %) (second %)))
         (filter #(= % \#))
         (count))))

(defn part-1 [right]
  (let [d       (read-data)
        w       (count (first d))
        indices (->> (range)
                     (map (comp #(mod % w) (partial * right))))]
    (->> (map #(get %1 %2) d indices)
         (filter #(= % \#))
         count)))

(defn ski [down right ]
  (let [rows (read-data)
        w    (count (first rows))
        rows (->> (partition down rows)
                  (map first))]
    (->> (range)
         (map (comp #(mod % w) #(* % right)))
         (map #(get %1 %2) rows)
         (filter #(= % \#))
         count)))

(defn part-1 []
  (ski 1 3))

(defn part-2 []
  (->>  [[1 1] [1 3] [1 5] [1 7] [2 1]]
        (map (partial apply ski))
        (reduce *)))
