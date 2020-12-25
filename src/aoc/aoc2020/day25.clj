(ns aoc.aoc2020.day25
  (:require [clojure.string :as s]))

(defn step [n k]
  (mod (* k n) 20201227))

(defn public-key [sn k]
  (->> (iterate (partial step sn) 1)
       (take-while (complement #{k}))
       (count)))

;; We can do ( k ^ loopsize mod step) instead
(defn secret-key [loopsize k]
  (->> (iterate (partial step k) 1)
       (drop loopsize)
       first))

(defn decrypt [k1 k2]
  (-> (public-key 7 k1)
      (secret-key k2)))

(defn test-1 []
  (let [[k1 k2] [17807724 5764801]]
    (prn (decrypt k2 k1))
    (decrypt k1 k2)))

(defn part-1 []
  (let [[k1 k2] [9281649 9033205]]
    (prn (decrypt k2 k1))
    (decrypt k1 k2)))
