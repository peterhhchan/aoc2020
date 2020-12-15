(ns aoc2020.day10)


(defn read-data []
  (->> (slurp "data/aoc2020_day10.txt")
       clojure.string/split-lines
       (map #(Long/parseLong %))))

(defn part-1 []
  (let [adapters     (sort (read-data))
        ;; Add the 0 and the +3
        adapters*   (cons 0 (into (list (+ 3 (apply max adapters))) (reverse adapters)) )
        f (->> (interleave adapters* (drop 1 adapters*))
               (partition 2)
               (map #(- (second %) (first %)))
               (frequencies))]

    (* (f 1) (f 3))))

(defn part-1 []
  ;; Alternate solution
  (let [adapters  (sort (read-data))
        f (->> (interleave adapters (drop 1 adapters))
               (partition 2)
               (map #(- (second %) (first %)))
               (frequencies))]
    (prn (first adapters))
    (prn (last adapters))
    f))


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
