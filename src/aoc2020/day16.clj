(ns aoc2020.day16
  (:require [clojure.string :as str]))

(defn read-data []
  (-> (slurp "data/aoc2020_day16.txt")
      (clojure.string/split #"\n\n" )))

(defn get-range [rr]
  (let [[n1 n2] (clojure.string/split rr #"-")]
    [(Integer/parseInt n1) (Integer/parseInt n2)]))

(defn parse-ticket [l]
  (->>  (clojure.string/split l #"," )
        (map #(Integer/parseInt %))))

(defn parse-rules [rules]
  (->> rules
       (clojure.string/split-lines)
       (map (fn [l]
              (let [[field ranges] (clojure.string/split l #": ")
                    [r1 r2]        (clojure.string/split ranges #" or ")]
                [field (get-range r1) (get-range r2)])))))

(defn valid-numbers [rules]
  (->> rules
       (map (fn [[_ [a b] [c d]]]
              (set (concat (range a (inc b))
                           (range c (inc d))))))
       (apply clojure.set/union)))

(defn part-1 []
  (let [[r _ others] (read-data)
        valid        (->> (parse-rules r)
                          (valid-numbers))]
    (->> others
         (clojure.string/split-lines)
         (drop 1)
         (map #(->> (parse-ticket %)
                    (remove valid)
                    (reduce +)))
         (apply +))))

(defn part-2 []
  (let [[rs t ns]     (read-data)
        rules         (parse-rules rs)
        valid-numbers (valid-numbers rules)
        ticket        (-> (clojure.string/split-lines t)
                           last
                           (parse-ticket)
                           vec)

        valid-tickets (->> (clojure.string/split-lines ns)
                            (drop 1)
                            (map parse-ticket)
                            (remove #(seq (remove valid-numbers %))))

        invalid-ranges (->> rules
                            (map (fn [[r [a b] [c d]]]
                                   [r (->> (concat (range 0 a)
                                                   (range (inc b) c)
                                                   (range (inc d) 999))
                                           (into #{}))])))
        valid-fields   (->> valid-tickets
                            (apply map vector)
                            (map #(into #{} %))
                            (map (fn [xs]
                                   (->> invalid-ranges
                                        (map (fn [[field inv]]
                                               (when (empty? (clojure.set/intersection xs inv))
                                                 field)))
                                        (remove nil?)
                                        (into #{})))))

        sorted-fields  (->> (map-indexed (fn [idx itm] {:pos idx :fields itm}) valid-fields)
                            (sort-by (comp count :fields)))]

    (->>     (interleave sorted-fields (rest sorted-fields))
             (partition 2)
             (map (fn [[f1 f2]]
                    {:pos    (:pos f2)
                     :fields (clojure.set/difference (:fields f2) (:fields f1))} ))
             (cons (first sorted-fields))
             (filter #(clojure.string/starts-with? (first (:fields %)) "departure" ))
             (map (comp ticket :pos))
             (reduce *))))
