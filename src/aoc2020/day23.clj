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
(def input [3 8 9 1 2 5 4 6 7])
(defn answer-1 [v]
  (->> (concat v v)
       (drop-while #(not= 1 %))
       (drop-while #{1})
       (take (dec (count v)))
       (apply str)))

(defn answer-2 [v]
  (->> (concat v v)
       (drop-while #(not= 1 %))
       (drop-while #{1})
       (take 2)))


(defn step-faster [all-cups]
  (let [current-cup  (first all-cups)
        cups         (rest all-cups)
        num-cups     (count all-cups)

        to-move      (take 3 cups)
        next-current (first (drop 3 cups))

        destination (->> (concat (range (dec current-cup) 0 -1)
                                 (range num-cups 0 -1))
                         (remove (set to-move) )
                         first)
        next-seq  (concat (drop 3 cups) (list current-cup))
        res       (partition-by #{destination} next-seq)]
    (cond (#{destination} (ffirst res))
          (concat (list destination) to-move (second res))
          (= 3 (count res))
          (concat (first res) (list destination) to-move (first (drop 2 res)))
          :else
          (rest res))))

(defn part-1 []
"25468379"
  (->> (iterate step-faster input)
       (drop 100)
       first
       answer-1))

(defn part-2 []
  (let [n 100000
        mx (apply max input)
        new-input (vec (concat input (range (inc mx) (inc n))))]
    (->> (iterate step-faster new-input)
         (drop 100)
         first
         answer-2)))

(defn take-cups [n {:keys [llist head tail]}]
  (loop [res     []
         cur     head
         counter 0]
    (if (= n counter)
      res
      (recur (conj res cur)
             (llist cur)
             (inc counter)))))

(defn cup-list [input]
  (loop [res {}
         cur nil
         [f & r] input]
    (if f
      (recur (assoc res f cur) f r)
      {:llist res
       :head (last input)
       :tail (first input)})))

;;[1 9 3 4 6 7 2 5 8]
;;(def input [3 8 9 1 2 5 4 6 7])

(defn debug-list [cup-list]
  (vec (take-cups (count (:llist cup-list)) cup-list)))

(defn insert-list [llist pos items]
;  (prn items)
  (let [cur (llist pos)]
    (-> llist
        (assoc pos (first items))
        (assoc (last items) cur))))

(defn get-tail [cup-list pos]
  (if (nil? (cup-list pos))
    pos
    (get-tail cup-list (cup-list pos))))

(defn step [{:keys [llist head tail] :as cup-list}]
  (let [fs           (take-cups 5 cup-list)
        to-move      (take 3 (drop 1 fs))
        next-head    (last fs)

        destination  (->> (concat (range (dec head) 0 -1)
                                  (range (count llist) 0 -1))
                          (remove (set to-move) )
                          first)
        next-seq  (-> llist
                      (assoc tail head)
                      (assoc head nil)
                      (insert-list destination to-move))]
    {:llist next-seq
     :head next-head
     :tail (get-tail next-seq head)}))

(def input [1 9 3 4 6 7 2 5 8])
;(def input [3 8 9 1 2 5 4 6 7])

#_(defn part-1 []
  (let [n 9
        mx (apply max input)
        new-input (vec (concat input (range (inc mx) (inc n))))
        ll (cup-list (reverse new-input))]
    (prn (count new-input))
    (->> (iterate step ll)
         (drop 100)
         first
         debug-list
         answer-1)))

(defn part-2 []
  (let [n 1000000
        mx (apply max input)
        new-input (vec (concat input (range (inc mx) (inc n))))
        ll (cup-list (reverse new-input))]
    (prn (count new-input))
    (->> (iterate step ll)
         (drop 10000000)
         first
         debug-list
         answer-2)))
