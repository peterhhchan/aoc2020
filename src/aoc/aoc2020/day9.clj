(ns aoc.aoc2020.day9)

(defn read-data []
  (->> (slurp "data/aoc2020_day9.txt")
       clojure.string/split-lines
       (map #(Long/parseLong %))))

(defn part-1
  []
  (let [numbers (vec (read-data))]
    (->> (range 25 (count numbers))
         (remove (fn [n]
                   (let [match (->>  (for [a (range 25)
                                           b (range 25)
                                           :when (< a b)]
                                       (+  (numbers (+ a (- n 25)))
                                           (numbers (+ b (- n 25)))))
                                     (filter #(= (numbers n) %)))]
                     (pos? (count match)))))
         first
         (get numbers))))

(defn consecutive? [n target numbers]
  (let [start-sum  (->> (range n)
                        (map #(get numbers %))
                        (apply +'))]
    (loop [index (dec n)
           sum   start-sum]
      (if (= target sum)
        [n index]
        (let [next (inc index)]
          (if (= next (count numbers))
            nil
            (recur next
                   (- (+ sum (numbers next))
                      (numbers (- next n))))))))))

(defn part-2 []
  (let [target    (part-1)
        numbers   (vec (read-data))
        [n index] (->> (range 2 (count numbers))
                       (map #(consecutive? % target numbers))
                       (remove nil?)
                       first)
        r         (->> (range (inc (- index n)) (inc index))
                       (map #(get numbers %)))]
    (+ (apply min r)
       (apply max r))))
