(ns aoc2020.day1)

(defn read-data []
  (->> (slurp "data/aoc2020_day1.txt")
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

(defn vectorize [n]
  (vec (make-array Integer n)))

(defn get-sums [my-hash nums n]
  (->> nums
       (filter #(get my-hash(- n %)))))


(defn part-1 []
  (let [n 2020
        my-hash (reduce #(assoc %1 %2 1)
                         (vectorize n)
                         data)]
    (first (get-sums my-hash data n))))

(defn part-2 []
  (let [data (read-data)
        my-hash (reduce #(assoc %1 %2 1)
                         (vectorize 2020)
                         data)]
    (->> (range (int (/ 2020 3)))
         (filter #(get my-hash %))
         (map #(get-sums my-hash data (- 2020 %)))
         (remove empty?)
         first)))
