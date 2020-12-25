(ns aoc.aoc2020.day25
  (:require [clojure.string :as s]))

(defn step [n k]
  (mod (* k n) 20201227))

(defn public-key [sn k]
  (->> (iterate (partial step sn) 1)
       (take-while (complement #{k}))
       (count)))

(defn secret-key [loopsize k]
  (->> (iterate (partial step k) 1)
       (drop loopsize)
       first))

#_(defn decrypt-test [k1 k2]
  (let [l1  (public-key 7 k1)
        l2  (public-key 7 k2)
        pw1 (secret-key l1 k2)
        pw2 (secret-key l2 k1)]
    [pw1 pw2]))

(defn decrypt [k1 k2]
  (-> (public-key 7 k1)
      (secret-key k2)))

(defn test-1 []
  (decrypt 5764801 17807724))

(defn part-1 []
  (decrypt 9033205 9281649))
