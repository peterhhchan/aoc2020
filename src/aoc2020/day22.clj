(ns aoc2020.day22
  (:require [clojure.string :as s]))


(defn toInt [s]
  (Integer/parseInt s))

(defn decks []
  (let [players (->> (s/split (slurp "data/aoc2020_day22.txt") #"\n\n")
                     (map #(->> (s/split-lines %)
                                (drop 1)
                                (mapv toInt))))]
 {:p1 (first players)
  :p2 (second players)}))

(defn toLong [s]
  (Long/parseLong s))

(defn play-game [d1 d2]
  (loop [deck1 d1
         deck2 d2]
    (let [c1 (first deck1)
          c2 (first deck2)]
      (cond (empty? deck1) [:p2 deck2]
            (empty? deck2) [:p1 deck1]
            :else
            (if (> c1 c2)
              (recur (conj (subvec deck1 1) c1 c2)
                     (subvec deck2 1))
              (recur (subvec deck1 1)
                     (conj (subvec deck2 1) c2 c1)))))))

(defn score-deck [d]
  (->> (reverse (range 1 (inc (count d))))
       (map * d)
       (reduce +)))

(defn part-1 []
  (let [{:keys [p1 p2]} (decks)]
    (-> (play-game p1 p2)
        (second)
        (score-deck))))

;; Better solution for part 1

(defn game [[d1 d2]]
  (let [c1 (first d1)
        c2 (first d2)]
    (if (> c1 c2)
      [(concat (rest d1) [c1 c2]) (rest d2)]
      [(rest d1) (concat (rest d2) [c2 c1])])))

(defn part-1b []
  (let [{:keys [p1 p2]} (decks)]
    (->> (iterate game [p1 p2])
         (drop-while (fn [[d1 d2]] (and (seq d1) (seq d2))))
         first
         (filter seq)
         first
         reverse
         (map * (next (range)))
         (reduce +))))


(def mem-game-2
  (memoize
   (fn [player1 player2 ]
     (loop [d1 (vec player1)
            d2 (vec player2)
            games-played #{}]
       (cond (empty? d2) {:winner :p1 :deck d1}
             (empty? d1) {:winner :p2 :deck d2}
             (games-played [d1 d2]) {:winner :p1 :deck d1}
             :else
             (let [f1  (d1 0)
                   f2  (d2 0)
                   sub-game? (and (<= f1 (dec (count d1)))
                                  (<= f2 (dec (count d2))))]
               (if (or (and sub-game?
                            (#{:p1} (:winner (mem-game-2 (subvec d1 1 (inc f1))
                                                         (subvec d2 1 (inc f2))))))
                       (and (not sub-game?)
                            (> f1 f2)))
                 (recur (conj (subvec d1 1) f1 f2)
                        (subvec d2 1)
                        (conj games-played [d1 d2]))
                 (recur (subvec d1 1)
                        (conj (subvec d2 1) f2 f1)
                        (conj games-played [d1 d2])))))))))

(defn part-2 []
  (let [{:keys [p1 p2]} (data)
        winner (mem-game-2 p1 p2)]
    (score-deck (:deck winner))))
