(ns aoc2020.day17
  (:require [clojure.string :as s]))

;; Method 1
;; My first attempt at this method, using nested hashmaps was almost 10x faster
(defn data []
  (-> (slurp "data/aoc2020_day17.txt")
      (s/split #"\n")))

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


(defn f-n [f n vs]
  (->> vs
       (map #(get % n))
       (apply f)))

(defn part-1-a []
  (let [d      (mapv vec (data))
        width  (count (first d))
        height (count d)
        world  (->> (for [x (range width)
                          y (range height)]
                     (when (= \# (get-in d [x y]))
                       {[x y 0] 1}))
                   (remove nil?)
                   (into {}))]
    (loop [n 6
           w world]
      (if (zero? n)
        (count (vals w))
        (let [minx (f-n min 0 (keys w))
              maxx (f-n max 0 (keys w))
              miny (f-n min 1 (keys w))
              maxy (f-n max 1 (keys w))
              minz (f-n min 2 (keys w))
              maxz (f-n max 2 (keys w))]
          (->> (for [i (range (dec minx) (+ 2 maxx))
                     j (range (dec miny) (+ 2 maxy))
                     k (range (dec minz) (+ 2 maxz))]
                 (let [ns (->> (neighbors-3d [i j k])
                               (map w)
                               (remove nil?)
                               count)]
                   (when (or (and (w [i j k]) (= 2 ns))
                             (= 3 ns))
                     {[i j k] 1})))
               (into {})
               (recur (dec n))))))))

(defn part-2-a [] ;; ~ 13s
  (let [d      (mapv vec (data))
        width  (count (first d))
        height (count d)
        world  (->> (for [x (range width)
                          y (range height)]
                     (when (= \# (get-in d [x y]))
                       {[x y 0 0] 1}))
                   (remove nil?)
                   (into {}))]
    (loop [n 6
           w world]
      (if (zero? n)
        (count (vals w))
        (let [minx (f-n min 0 (keys w))
              maxx (f-n max 0 (keys w))
              miny (f-n min 1 (keys w))
              maxy (f-n max 1 (keys w))
              minz (f-n min 2 (keys w))
              maxz (f-n max 2 (keys w))]

          (->> (for [i (range (dec minx) (+ 2 maxx))
                     j (range (dec miny) (+ 2 maxy))
                     k (range (dec minz) (+ 2 maxz))
                     ;; z and w are symmetrical
                     l (range (dec minz) (+ 2 maxz))]
                 (let [ns (->> (neighbors-4d [i j k l])
                               (map w)
                               (remove nil?)
                               count)]
                   (when (or (and (w [i j k l]) (= 2 ns))
                             (= 3 ns))
                     {[i j k l] 1})))
               (into {})
               (recur (dec n))))))))



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

(defn next-generation [f alive]
  ;; Naive way
  (->> (set (mapcat f alive))
       (into alive)
       (map (fn [p]
              (let [n (->> (f p)
                           (filter alive)
                           count)]
                (when (or (and (alive p) (= n 2))
                          (= n 3))
                  p))))
       (remove nil?)
       (set)))

(defn next-generation [f alive]
  ;; Clever way !!!
  (->> (mapcat f alive)
       (frequencies)
       (map (fn [[pos n]]
              (when (or (and (alive pos) (= n 2))
                        (= n 3))
                pos)))
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
