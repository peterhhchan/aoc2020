(ns aoc2020.day15)

(def input [0 3 6])


(defn part-1
  [input steps]
  (loop [step (count input)
         said (zipmap (butlast input)
                      (map inc (range)))
         last (last input)]
     (if (= step steps)
      last
      (let [next (if-let [l (get said last)]
                   (- step l)
                   0)]
        (recur (inc step)
               (assoc said last step)
               next)))))
