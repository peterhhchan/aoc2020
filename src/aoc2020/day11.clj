(ns aoc2020.day11)

(defn read-data []
  (->> (slurp "data/aoc2020_day11.txt")
       clojure.string/split-lines
       (mapv vec)))

(def seats
  (->> (for [x [-1 0 1]
             y [-1 0 1]
             :when (not= x y 0)]
         [x y])))

(defn adj-seats [seating [r c] n]
  (->> seats
       (map (fn [[a b]]
              (->> (if n (range n) (range))
                   (map inc)
                   (map #(get-in seating [(+ (* % a) r)
                                          (+ (* % b) c)]))
                   (take-while #(or (= % \#)
                                    (= % \.)
                                    (= % \L)))
                   (filter #(or (= % \#)
                                (= % \L)))
                   first)))
       (frequencies)))

(defn update-seats [cur d n]
  (->>  (for [r (range (count cur))
              c (range (count (first cur)))]
          (let [adj  (adj-seats cur [r c] d)]
            (case  (get-in cur [r c])
              \L (if (= 0 (get adj \# 0)) \# \L)
              \. \.
              \# (if (>= (get adj \# 0) n) \L \#))))
        (partition (count (first cur)))
        (mapv vec)))

(defn final-seating [d n]
  (->> (loop [curr (read-data)]
         (let [next (update-seats curr d n)]
           (if (= curr next)
             curr
             (recur next ))))
       (flatten)
       (filter #(= % \#))
       count))

(defn part-1 []
  (final-seating 1 4))

(defn part-2 []
  (final-seating nil 5))
