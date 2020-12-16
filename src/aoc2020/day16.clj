(ns aoc2020.day16)

(defn read-data []
  (-> (slurp "data/aoc2020_day16.txt")
      (clojure.string/split #"\n\n" )))

(defn get-range [rr]
  (let [[n1 n2] (clojure.string/split rr #"-")]
    [(Integer/parseInt n1) (Integer/parseInt n2)]))

(defn part-1 []
  (let [[r _ others]  (-> (slurp "data/aoc2020_day16.txt")
                          (clojure.string/split #"\n\n"))
        valid        (->> r
                          (clojure.string/split-lines)
                          (map (fn [l]
                                 (let [[field ranges] (clojure.string/split l #": ")
                                       [r1 r2]         (clojure.string/split ranges #" or ")]
                                   [field (get-range r1) (get-range r2)])))
                          (map (fn [[_ [a b] [c d]]]
                                 (set (concat (range a (inc b))
                                              (range c (inc d))))))
                          (apply clojure.set/union))]
    (->> others
         (clojure.string/split-lines)
         (drop 1)
         (map (fn [l]
                (->> (clojure.string/split l #",")
                     (map #(Integer/parseInt %))
                     (into #{})
                     (remove valid)
                     (reduce +))))
         (apply +))))

(defn part-2 []
  (let [[r t nearby] (read-data)
        rules        (->> r
                          (clojure.string/split-lines)
                          (map (fn [l]
                                 (let [[policy ranges] (clojure.string/split l #": ")
                                       [r1 r2] (clojure.string/split ranges #" or ")]
                                   [policy (rr r1) (rr r2)]))))
        t (mapv #(Integer/parseInt %) (-> t
                                         (clojure.string/split-lines)
                                         second
                                         (clojure.string/split #",")
                                         vec))
        others (->> (clojure.string/split-lines nearby)
                    (drop 1)
                    (map #(clojure.string/split % #","))
                    (map (fn [l]
                           (set (map #(Integer/parseInt %) l)))))
        valid     (->> rules
                       (map (fn [[_ [a b] [c d]]]
                              (set (flatten
                                    [(range a (inc b))
                                     (range c (inc d))]))))
                       (apply clojure.set/union))
        valid-nearby    (->> others
                             (remove (fn [ns]
                                       (pos?
                                        (->> (remove valid ns)
                                             (reduce +))))))
        nearby (->> (clojure.string/split-lines nearby)
                    (drop 1)
                    (map #(clojure.string/split % #","))
                    (map (fn [l]
                           (map #(Integer/parseInt %) l))))
        invalid-ranges     (->> rules
                                (map (fn [[r [a b] [c d]]]
                                       [r (set (flatten [(range (inc b) c)]))])))
        fields     (->> nearby
                        (apply map vector)
                        (map #(into #{} %))
                        (map (fn [ns]
                               (->> invalid-ranges
                                    (map (fn [[r-name r-set]]
                                           (when (zero? (count (clojure.set/intersection ns r-set)))
                                             r-name)))
                                    (remove nil?)
                     (into #{})))))
        my-seq (->> (zipmap(range) fields)
                    (map (fn [[k v]]
                           [k (count v) v]))
                    (sort-by second))]
    (->>     (interleave my-seq (rest my-seq))
             (partition 2)
             (map (fn [[[a b c] [d e f]]]
                    [d (clojure.set/difference f c) ])))

    (->>     (map  #(get t %) [6 11 5 10 16 19])
             (reduce *))))
