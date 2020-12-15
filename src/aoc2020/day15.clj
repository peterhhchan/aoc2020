(ns aoc2020.day15)

(def input [0 3 6])
(def input [5 1 9 18 13 8 0])

(defn part-1
  [stop]
  (loop [n      (count input)
         memory (zipmap input (range))
         cur    0]
    (let [next-number (if (get memory cur)
                        (- n (get memory cur))
                        0)]
      (if (= (inc n) stop)
        cur
        (recur (inc n)
               (assoc memory cur n)
               next-number)))))
