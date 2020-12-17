(ns aoc2020.day17
  (:require [clojure.string :as s]))

(defn toInt [s]
  (Integer/parseInt s)

(defn toLong [s]
  (Long/parseLong s)))

(defn data []
  (-> (slurp "data/aoc2020_day17.txt")
      (s/split #"\n")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Method 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-data [n]
  (let [alives   (->> (data)
                      (apply map vector)
                      vec)]
    (->> (for [r (range (count alives))
               c (range (count (first alives)))]
           (when (= \# (get-in alives [r c]))
             [r c]))
         (remove nil?)
         (map #(into % (repeat n 0)))
         set)))

(def ds [-1 0 1])
(defn neighbors-3d [[a b c]]
 (for [x ds
       y ds
       z ds
       :when (and (not= 0 x y z))]
    [(+ a x) (+ b y) (+ c z)]))

(defn neighbors-4d [[a b c d]]
 (for [x ds
       y ds
       z ds
       w ds
       :when (and (not= 0 x y z w))]
    [(+ a x) (+ b y) (+ c z) (+ d w)]))

(defn next-generation [f alive]
  (->> (set (mapcat f alive))
       (into alive)
       (map (fn [p]
              (let [n (->> (f p)
                           (filter alive)
                           count)]
                (when (or (and (alive p)
                               (or (= n 2) (= n 3)))
                          (and (not (alive p))
                               (= n 3)))
                  p))))
       (remove nil?)
       (set)))

(defn part-1 []
  (->> (make-data 1)
       (iterate (partial next-generation neighbors-3d))
       (drop 6)
       first
       count))

(defn part-2 []
  (->> (make-data 2)
       (iterate (partial next-generation neighbors-4d))
       (drop 6)
       first
       count))
