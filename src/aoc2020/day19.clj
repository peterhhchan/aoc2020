(ns aoc2020.day19
  (:require [clojure.string :as s]
            [clojure.string :as str]
            [clojure.set :as set]))

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
                             (= 3 (count rule))
                             (into []
                                   (for [x (merge-rule lib (get rule 0))
                                         y (merge-rule lib (get rule 1))
                                         z (merge-rule lib (get rule 2))]
                                     (str x y z)))))))))))

(defn rules-dict [rules]
  (->> rules
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
       (into {})))

(defn part-1 []
  (let [[rules msgs] (data)
        lib          (rules-dict rules)
        all-rules    (set (merge-rule lib 0))]
    (->> msgs
         (s/split-lines)
         (filter all-rules)
         count)))

(defn part-2 []
  (let [[rules msgs] (data)
        lib          (rules-dict rules)
        heads     (set (merge-rule lib 42))
        tails     (set (merge-rule lib 31))
        all-rules (set (merge-rule lib 0))
        len       (count (first heads))]

    (prn (count (first heads)))
    (prn (count (s/split-lines msgs)))
    (prn (count (set/intersection heads tails)))

    (->> msgs
         (s/split-lines)
         (map (fn [msg]
                (->> (partition len msg)
                     (map #(let [ss (apply str %)]
                             (cond (heads ss) :a
                                   (tails ss) :b
                                   :else nil)))
                     (partition-by identity))))
         (filter (fn [msg]
                   (and (= 2 (count msg))
                        (= :a (ffirst msg))
                        (= :b (first (last msg)))
                        (> (count (first msg)) (count (second msg))))))
         (count))))
