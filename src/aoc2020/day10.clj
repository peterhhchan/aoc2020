(ns aoc2020.day10)


(defn read-data []
  (->> (slurp "data/aoc2020_day10.txt")
       clojure.string/split-lines
       (map #(Long/parseLong %))))

(defn part-1 []
  (let [adapters (cons 0 (sort (read-data)))
        f        (->> (map - (next adapters) adapters)
                    (frequencies))]
    (->> (update f 3 inc)
         vals
         (apply *))))


(defn part-2 []
  (let [adapters (vec (cons 0 (sort (read-data))))
        total    (count adapters)]
    (loop [n           (- total 2)
           connections {(dec total) 1}]
      (if (< n 0)
        (get connections 0)
        (let [cs (->> (range 1 4)
                      (map (partial + n))
                      (filter (fn [idx]
                                (and (get adapters idx)
                                     (>= (+ 3 (get adapters n))
                                         (get adapters idx)))))

                      (map #(get connections %))
                      (reduce +))]
          (recur (dec n) (assoc connections n cs)))))))
