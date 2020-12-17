(ns aoc2020.day17
  (:require [clojure.string :as s]))

(defn toInt [s]
  (Integer/parseInt s)

(defn toLong [s]
  (Long/parseLong s)))

(defn data []
  (-> (slurp "data/aoc2020_day17.txt")
      (s/split #"\n")))


(defn nns [[a b c]]
  (for [x [-1 0 1]
        y [-1 0 1]
        z [-1 0 1]
        :when (and (not= 0 x y z))]
    [(+ a x)
     (+ b y)
     (+ c z)]))


(defn count-s [w]
 (->> (let [mn (apply min (keys w))
            mx (apply max (keys w))]
        (->> (for [i (range mn (+ 1 mx))
                   j (range mn (+ 1 mx))
                   k (range -6 7)]
               (get-in w [i j k]))))
      (frequencies)))

(defn part-1 []
  (let [d     (mapv vec (data))
        world (->> (for [x (range (count d))
                         y (range (count d))]
                     [[x y] (get-in d [x y])])
                   (reduce (fn [c [[x y] v]]
                             (assoc-in c [x y 0] v))
                           {}))
        res   (loop [n 6
                     w world]
                (if (zero? n)
                  w
                  (let [mn (apply min (keys w))
                        mx (apply max (keys w))]
                    (->> (for [i (range (dec mn) (+ 2 mx))
                               j (range (dec mn) (+ 2 mx))
                               k (range -6 7)]
                           (let [is (get-in w [i j k] \.)
                                 ps (->> (nns [i j k])
                                         (map #(get-in w % \.))
                                         (frequencies))
                                 nv (cond (and (= is \.)
                                               (= (ps \#) 3))
                                          \#
                                          (and (= is \#)
                                               (or (= 2 (ps \#))
                                                   (= 3 (ps \#))))
                                          \#
                                          :else
                                          \.)]
                             [[i j k] nv]))

                         (reduce (fn [col [pos v]]
                                   (assoc-in col pos v))
                                 {})
                         (recur (dec n))))))]
    (prn (count-s world))
    (prn (count-s res))))

(defn count-ss [w]
 (->>    (let [mn (apply min (keys w))
               mx (apply max (keys w))]
              (->> (for [i (range mn (+ 1 mx))
                         j (range mn (+ 1 mx))
                         k (range -6 7)
                         l (range -6 7)]
                     (get-in w [i j k l]))))
            (frequencies)))

(defn nnns [[a b c d]]
  (for [x [-1 0 1]
        y [-1 0 1]
        z [-1 0 1]
        w [-1 0 1]
        :when (and (not= 0 x y z w))]
    [(+ a x)
     (+ b y)
     (+ c z)
     (+ d w)]))


(defn part-2 []
  (let [d     (mapv vec (data))
        world (->> (for [x (range (count d))
                         y (range (count d))]
                     [[x y] (get-in d [x y])])
                   (reduce (fn [c [[x y] v]]
                             (assoc-in c [x y 0 0] v))
                           {}))
        res   (loop [n 6
                     w world]
                (if (zero? n)
                  w
                  (let [mn (apply min (keys w))
                        mx (apply max (keys w))]
                    (->> (for [i (range (dec mn) (+ 2 mx))
                               j (range (dec mn) (+ 2 mx))
                               k (range -6 7)
                               l (range -6 7 )]
                           (let [is (get-in w [i j k l] \.)
                                 ps (->> (nnns [i j k l])
                                         (map #(get-in w % \.))
                                         (frequencies))
                                 nv (cond (and (= is \.)
                                               (= (ps \#) 3))
                                          \#
                                          (and (= is \#)
                                               (or (= 2 (ps \#))
                                                   (= 3 (ps \#))))
                                          \#
                                          :else
                                          \.)]
                             [[i j k l] nv]))

                         (reduce (fn [col [pos v]]
                                   (assoc-in col pos v))
                                 {})
                         (recur (dec n))))))]
    (prn (get-in world [ 0 0 0 0]))
    (prn (count-ss world))
    (prn (count-ss res))))
