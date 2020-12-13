(ns aoc2020.day3)

(defn read-data []
  (->> (slurp "data/aoc2020_day3.txt")
       clojure.string/split-lines))

(defn day3 []
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

(defn day3-1 [right]
  (let [d       (read-data)
        w       (count (first d))
        indices (->> (range)
                     (map (comp #(mod % w) (partial * right))))]
    (->> (map #(get %1 %2) d indices)
         (filter #(= % \#))
         count)))

(defn ski [down right ]
  (let [d       (read-data)
        w       (count (first d))
        indices (->> (range)
                     (map (comp #(mod % w) (partial * right))))
        rows    (->> (partition down d)
                     (map first))]

    (->> (map #(get %1 %2) rows indices)
         (filter #(= % \#))
         count)))


(defn day3-2 []
  (let [a (ski 1 1)
        b (ski 1 3)
        c (ski 1 5)
        d (ski 1 7)
        e (ski 2 1)]
    (* a b c d e)))
