(ns aoc2020.day19
  (:require [clojure.string :as s]
            [clojure.string :as str]))

(defn data []
  (-> (slurp "data/aoc2020_day19.txt")
       (s/split #"\n\n")))

(def test-input
  ["0: 4 1 5"
   "1: 2 3 | 3 2"
   "2: 4 4 | 5 5"
   "3: 4 5 | 5 4"
   "4: \"a\""
   "5: \"b\""])

(defn toInt [s]
  (Integer/parseInt s))

(defn toLong [s]
  (Long/parseLong s))

(defn isLetter? [s]
  (re-find #"\"([ab]+)\"" s))

(defn merge-rule
  ([lib ]
   (merge-rule lib 0))

  ([lib rid]
   (let [rules (get lib rid "")]
     (if (string? rules)
       [rules]
       (->> rules
            (mapcat (fn [rule]
                      (cond  (= 1 (count rule))
                             (merge-rule lib (get rule 0))
                             (= 2 (count rule))
                             (into []
                                   (let [r1 (merge-rule lib (get rule 0))
                                         r2 (merge-rule lib (get rule 1))]
                                     (for [x r1
                                           y r2]
                                       (str x y))))
#_                             (= 3 (count rule))
#_
                             (into []
                                   (for [x (merge-rule lib (get rule 0))
                                         y (merge-rule lib (get rule 1))
                                         z (merge-rule lib (get rule 2))]
                                     (str x y z)))))))))))

(defn part-1 []
  (let [[rules msgs] (data)
        lib (->> rules
                 (s/split-lines)
                 (map (fn [rule]
                        (let [[id rs] (s/split rule #": ")
                              rid (toInt id)]
                          {rid
                           (if (isLetter? rs)
                             (second (isLetter? rs))
                             (->>  (s/split rs #" \| ")
                                   (mapv (fn [r]
                                          (->> (s/split r #" ")
                                               (mapv toLong))))))})))
                 (into {}))
        all-rules (set (merge-rule lib 0))]
    (->> msgs
         (s/split-lines)
         (filter all-rules)
         count)))


(defn part-2 []
  (let [[rules msgs] (data)
        lib (->> rules
                 (s/split-lines)
                 (map (fn [rule]
                        (let [[id rs] (s/split rule #": ")]
                          {(toInt id)
                           (if (isLetter? rs)
                             (second (isLetter? rs))
                             (->>  (s/split rs #" \| ")
                                   (mapv (fn [r]
                                          (->> (s/split r #" ")
                                               (mapv toLong))))))})))
                 (into {}))
        new-rule (set (merge-rule lib 8))
        all-rules (set (merge-rule lib 0))
        unmatched     (->> msgs
                     (s/split-lines)
                     (remove all-rules))]
    (->>     msgs
             (s/split-lines)
             (filter all-rules))
#_    (->> unmatched
         (filter (fn [s]
                (some #(str/includes? s %) new-rule)))
         count)))


#_(defn part-2 []
  (let [[rules msgs] (data)
        lib (->> rules
                 (s/split-lines)
                 (map (fn [rule]
                        (let [[id rs] (s/split rule #": ")
                              rid (toInt id)]
                          {rid
                           (if (isLetter? rs)
                             (second (isLetter? rs))
                             (->>  (s/split rs #" \| ")
                                   (mapv (fn [r]
                                          (->> (s/split r #" ")
                                               (mapv toLong))))))})))
                 (into {}))
        ra (set (merge-rule lib 8))
        rb (set (merge-rule lib 42))
        rc (set (merge-rule lib 31))

        r-all (set (merge-rule lib 0))
        max-length (apply max (map count r-all))]


    (->> msgs
         (s/split-lines)
         (filter (fn [s]
                   (or (r-all s)
                       (some? (map #(r-all (str % s)) ra))
                       (seq (->> (for [x rb
                                       y rc]
                                   (str x s y))
                                 (filter r-all)
                                 (remove nil?))))))
         (count)
)

))
