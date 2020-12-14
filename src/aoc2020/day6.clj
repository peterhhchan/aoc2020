(ns aoc2020.day6)

(defn groups []
(-> (slurp "data/aoc2020_day6.txt")
    (clojure.string/split #"\n\n")))

(defn part-1 []
  ;; This solution is cheating
  (->> (groups)
       (map #(-> (frequencies %)
                 (dissoc \newline) keys count))
       (reduce +)))

(defn part-1 []
  (->> (groups)
       (map #(-> (into #{} (seq %))
                 (disj \newline) keys count))
       (reduce +)))


(defn part-2 []
  ;; This solution is cheating
  (->> (groups)
       (map (fn [g]
              (let [n (count (clojure.string/split-lines g))]
                (->> (dissoc (frequencies g) \newline)
                     (filter #(= n (second %)))
                     count))))
       (reduce +)))
