(ns aoc2020.day1)

(defn read-data []
  (->> (slurp "data/aoc2020_day1.txt")
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

(defn my-array [n]
  (vec (make-array Integer n)))

(defn valid-entries [entries xs y]
  (filter #(get entries (- y %)) xs))

(defn part-1 []
  (let [y       2020
        data    (read-data)
        my-hash (reduce #(assoc %1 %2 1)
                        (my-array y)
                        data)
        n       (first (valid-entries my-hash data y))]

    (* n (- y n))))


(defn part-2 []
  (let [data    (read-data)
        my-hash (reduce #(assoc %1 %2 1)
                        (my-array 2020)
                        data)
        ns      (->> (range (int (/ 2020 3)))
                     (filter #(get my-hash %))
                     (map #(valid-entries my-hash data (- 2020 %)))
                     (remove empty?)
                     first)]
    (* (- 2020 (reduce + ns))
       (reduce * ns))))

;; O(n) solution for part 1.  Only iterates the input once (not
;; including the initial `split-lines` in read-data)
(defn part-1 []
  (let [y 2020]
    (loop [t       (my-array y)
           [f & r] (read-data)]
      (if (get t (- y f))
        (* f (- y f))
        (when (seq r)
          (recur (assoc t f 1) r ))))))
