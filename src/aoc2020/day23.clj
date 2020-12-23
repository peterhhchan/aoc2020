(ns aoc2020.day23
  (:require [clojure.string :as s]))

(defn data []
  (->> (slurp "data/aoc2020_day23.txt")
       s/split-lines))

(defn toInt [s]
  (Integer/parseInt s))

(defn toLong [s]
  (Long/parseLong s))

(defn part-1 []
  (let [d (data)]))

(defn part-2 []
  (let [d (data)]))

(def input [1 9 3 4 6 7 2 5 8])
;(def input [3 8 9 1 2 5 4 6 7])

(defn step-cups [[current cups n]]
  (let [current-idx (.indexOf cups current)
        to-move     (-> (into cups cups)
                        (subvec (inc current-idx) (+ 4 current-idx)))
        remaining   (vec (remove (set to-move) cups))
        destination (->> (concat (range (dec current) 0 -1)
                                 (range n 0 -1))
                         (filter (set remaining) )
                         first)
        dest-idx    (.indexOf remaining destination)
        [l r]       (split-at dest-idx remaining )
        new-order   (concat [(first r)] to-move (rest r) l)
        new-order   (concat new-order new-order)
        drop-n      (mod (- (.indexOf new-order current)
                            (.indexOf cups current))
                    (count cups))
        new-current (get (vec new-order) (inc (.indexOf new-order current))) ]
    [new-current
     (->> new-order
          (drop drop-n)
          (take (count cups))
          vec)
     n]))

(defn make-answer [v]
  (->> (concat v v)
       (drop-while #(not= 1 %))
       (drop-while #{1})
       (take (dec (count v)))
       (apply str)))

(defn part-1 []
"25468379"
  (->> [(first input) input (apply max input)]
       (iterate step-cups)
       (drop 100)
       first
       second
       make-answer))

(defn answer-2 [v]
  (->> (concat v v)
       (drop-while #(not= 1 %))
       (drop-while #{1})
       (take 2)))

(defn step-cups-2 [[current cups n magic-index]]
  (let [current-idx magic-index
        bigvec      (vec (into cups cups))
        to-move     (-> bigvec
                        (subvec (inc current-idx) (+ 4 current-idx)))

        remaining  (if (< (+ 4 current-idx) n)
                     (vec (concat (subvec cups 0 (inc current-idx))
                                  (subvec cups (+ 4 current-idx))))
                     (subvec cups
                             (max 0 (- (+ 4 current-idx) n))
                             (min n (inc current-idx))))
;        remaining   (vec (remove (set to-move) cups))

        destination       (->> (concat (range (dec current) 0 -1)
                                       (range n 0 -1))
                               (remove (set to-move) )
                               first)
        dest-idx    (.indexOf remaining destination)
        [l r]       (split-at dest-idx remaining )
        new-order   (concat [(first r)] to-move (rest r) l)
        new-order   (concat new-order new-order)
        find-idx    (.indexOf new-order current)
        drop-n      (mod (- find-idx
                            magic-index)
                         (count cups))
        new-current (get (vec new-order) (inc find-idx)) ]

    [new-current
     (->> new-order
          (drop drop-n)
          (take (count cups))
          vec)
     n
     (inc magic-index)]))

(defn part-2 []
  (let [mx (apply max input)
        n 5000
        new-input (vec (concat input (range (inc mx) (inc n))))]
  (->> [(first new-input) new-input n 0]
       (iterate step-cups-2)
       (drop n)
       first
       second
       answer-2)))
