(ns aoc.aoc2020.day5)

(defn read-data []
  (->> (slurp "data/aoc2020_day5.txt")
       clojure.string/split-lines))

(defn find-loc [mn mx zone]
  (loop [x mn
         y mx
         [f & r] zone]
    (let [rows (inc (- y x))]
      (cond
        (or (= f \L)
            (= f \F))
        (recur x
               (dec (+ x (/ rows 2)))
               r)
        (or (= f \B)
            (= f \R))
        (recur (+ x (/ rows 2)) y r)
        :else
        x))))

(defn seat-id [id]
  (let [[row col] (split-at 7 id)]
    (+ (* 8 (find-loc 0 127 row))
       (find-loc 0 7 col))))

(defn tests []
  (let [ids ["BFFFBBFRRR"
             "FFFBBBFRRR"
             "BBFFBBFRLL"
             "FBFBBFF"]]
    (map seat-id ids)))

(defn part-1 []
  (->> (read-data)
       (map seat-id)
       (apply max)))

(defn sum-to [n]
  (* n (inc n) 0.5))

(defn part-2 []
  ;; Not the optimal solution
  ;; We can calulate the sum, min and max in a single pass
  (let [ids  (->> (read-data)
                  (map seat-id))
        mn   (apply min ids)
        mx   (apply max ids)]

    (- (apply + ids)
       (- (sum-to mx)
          (sum-to (dec mn))))))
