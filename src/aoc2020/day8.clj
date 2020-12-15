(ns aoc2020.day8)

(defn read-data []
  (->> (slurp "data/aoc2020_day8.txt")
       clojure.string/split-lines
       (map (fn [l]
              (let [[i v] (clojure.string/split l #" ")]
                [i (Integer/parseInt v)])))
       (zipmap (range))))

(defn part-1 []
  (let [instructions (read-data)]
    (loop [n 0
           acc 0
           visited #{}]
      (if (visited n)
        acc
        (let [[i v] (instructions n)]
          (cond (= "jmp" i)
                (recur (+ v n) acc (conj visited n))
                (= "acc" i)
                (recur (inc n) (+ acc v) (conj visited n))
                (= "nop" i)
                (recur (inc n) acc (conj visited n))))))))

(defn terminates? [instructions n]
  (let [[i v]            (instructions n)
        new-instructions (if (= "jmp" i)
                            (assoc instructions n ["nop" v])
                            (assoc instructions n ["jmp" v]))]
    (loop [n 0
           acc 0
           visited #{}]
      (if (visited n)
        nil
        (if (= n (count instructions))
          acc
          (let [[i v] (new-instructions n)]
            (cond (nil? i)
                  nil
                  (= "jmp" i)
                  (recur (+ v n) acc (conj visited n))
                  (= "acc" i)
                  (recur (inc n) (+ acc v) (conj visited n))
                  (= "nop" i)
                  (recur (inc n) acc (conj visited n)))))))))

(defn part-2 []
  (let [instructions (read-data)
        to-test     (loop [n 0
                           acc 0
                           visited {}
                           x 0]
                      (if (visited n)
                        visited
                        (let [[i v] (instructions n)]
                          (cond (= "jmp" i)
                                (recur (+ v n) acc (assoc visited n x) (inc x))
                                (= "acc" i)
                                (recur (inc n) (+ acc v)  (assoc visited n x) (inc x) )
                                (= "nop" i)
                                (recur (inc n) acc  (assoc visited n x) (inc x))))))]
    (->> to-test
         (sort-by second)
         (map first)
         (remove #(= "acc" (first (instructions %))))
         (map (partial terminates? instructions))
         (remove nil?)
         first)))
