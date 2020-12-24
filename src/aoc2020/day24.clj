(ns aoc2020.day24)

(defn tile [t]
  (let [f     (frequencies t)
        ew    (- (get f "e" 0) (f "w" 0))
        ns    (- (+ (get f "nw" 0) (get f "ne" 0))
                 (+ (get f "sw" 0) (get f "se" 0)))
        ew-ns (- (+ (get f "ne" 0) (get f "se" 0))
                 (+ (get f "nw" 0) (get f "sw" 0)))]
    [(+ ew (/ ew-ns 2)) ns]))

(defn tiles []
  (->> (slurp "data/aoc2020_day24.txt")
       s/split-lines
       (map #(map first (re-seq #"(se|sw|ne|nw|e|w)" %)))
       (map tile)))

(defn black? [f] (odd? (second f)))

(defn part-1 []
  (->> (tiles)
       (frequencies)
       (filter black?)
       count))

(defn neighbors [[x y]]
  (->> (zipmap ["ne" "nw" "se" "sw" "e" "w"] (range))
       (map tile)
       (mapv (fn [[a b]]
              [(+ a x) (+ b y)]))))

(defn step [blacks]
  (->> (mapcat neighbors blacks)
       (frequencies)
       (filter (fn [[t cnt]]
                 (or (and (blacks t)
                          (#{1 2} cnt))
                     (#{2} cnt))))
       (map first)
       set))

(defn part-2 []
  (->> (tiles)
       (frequencies)
       (filter black?)
       (map first)
       set
       (iterate step)
       (drop 100)
       first
       count))
