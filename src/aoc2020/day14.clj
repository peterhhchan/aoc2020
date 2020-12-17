(ns aoc2020.day14
  (:require [clojure.string :as s]))

(defn data []
  (-> (slurp "data/aoc2020_day14.txt")
      (s/split #"\n")))

(defn read-mask [l]
  (second (first (re-seq #".* = ([01X]+)" l))))

(defn splat [i]
  (let [ss  (Integer/toBinaryString i)
        pre (s/join  (repeat (- 36 (count ss)) "0"))]
    (str pre ss)))

(defn xx [m v]
  (->> (map (fn [mask wr]
                (cond (= mask \1)
                      \1
                      (= mask \0)
                      \0
                      :else
                      wr))
              m v)
       (s/join)))

(defn read-write [l]
  (let [[[a b c]] (re-seq #"mem\[([\d]+)\] = ([\d]+)" l)]
    [(Integer/parseInt b) (splat (Integer/parseInt c))]))

(defn part-1 []
  (let [cs (->> (data)
                (partition-by #(s/starts-with? % "mask"))
                (partition 2)
                (map (fn [[m ws]]
                       {:mask   (read-mask (first m))
                        :writes (map read-write ws)}))
                (map (fn [{:keys [mask writes]}]
                       (->> writes
                            (map (fn [[a v]]
                                   {a (Long/parseLong (xx mask v) 2)}))
                            (into {}))))
                (apply merge))]
    (->>     (map second cs)
             (reduce +))))

(defn xxx [m v]
  (->> (map (fn [mask wr]
                (cond (= mask \1)
                      \1
                      (= mask \0)
                      wr
                      :else
                      \X))
              m v)
       (s/join)))

(defn read-write-2 [l]
  (let [[[a b c]] (re-seq #"mem\[([\d]+)\] = ([\d]+)" l)]
    [(splat (Integer/parseInt b)) (Integer/parseInt c)]))

(defn splat-2 [a]
  (let [nx (get (frequencies a) \X 0)]
    (loop [n nx
           as [a]]
      (if (zero? n)
        as
        (recur (dec n)
               (mapcat (fn [av]
                         [(s/replace-first av "X" "0")
                          (s/replace-first av "X" "1")])
                       as))))))

(defn part-2 []
  (let [cs (->> (data)
                (partition-by #(s/starts-with? % "mask"))
                (partition 2)
                (map (fn [[m ws]]
                       {:mask   (read-mask (first m))
                        :writes (map read-write-2 ws)}))
                (map (fn [{:keys [mask writes]}]
                       (->> writes
                            (map (fn [[a v]]
                                   #_[(xxx mask a) v]
                                   (->> (xxx mask a)
                                        (splat-2)
                                        (map (fn [a]
                                               {a v}))
                                        (into {}))))
                            (apply merge))))
                (apply merge))]
    (->>     (map second cs)
             (reduce +))))
