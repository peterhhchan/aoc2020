(ns aoc2020.day12)

(defn read-data []
  (->> (slurp "data/aoc2020_day12.txt")
       clojure.string/split-lines
       (map (fn [s]
              [(first s) (Integer/parseInt (subs s 1))]))))

(def directions [\N \E \S \W])
(def dir (zipmap directions (range)))

(defn update-direction [d x]
  (get directions (mod (- (dir d) x) (count directions))))


(defn data []
  (->> ["F10" "N3" "F7" "R90" "L90" "R90" "R180" "F10" "L180" "F10" "F11"]
       (map (fn [s]
              [(first s) (Integer/parseInt (subs s 1))]))))

(defn part-1 []
  (let [commands (read-data)]
    (loop [[command & r] commands
           x 0
           y 0
           f \E]
      (if-let [[action v] command]
        (cond (= action \N)
              (recur r x (+ v y) f)
              (= action \S)
              (recur r x (- y v) f)
              (= action \E)
              (recur r (+ x v) y f)
              (= action \W)
              (recur r (- x v) y f)
              (= action \F)
              (recur (cons [f v] r) x y f)
              (= action \L)
              (let [delta (/ v 90)]
                (recur r x y (update-direction f delta) ))
              (= action \R)
              (let [delta (/ v 90)]
                (recur r x y (update-direction f delta) )))
        (+ (Math/abs y) (Math/abs x))))))

(defn rotate [[x y] n]
  (get [[x y]
        [(- y) x]
        [(- x) (- y)]
        [y (- x)]]
       (mod n 4)))

(defn part-2 []
  (let [commands (read-data)]
    (loop [[command & r] commands
           x 0
           y 0
           wx 10
           wy -1]
      (if-let [[action v] command]
        (cond (= action \N)
              (recur r x y
                     wx (- wy v))
              (= action \S)
              (recur r x y
                     wx (+ wy v) )
              (= action \E)
              (recur r x y
                     (+ wx v) wy)
              (= action \W)
              (recur r x y
                     (- wx v) wy)
              (= action \F)
              (recur r
                     (+ x (* wx v))
                     (+ y (* wy v))
                     wx wy)
              (= action \L)
              (let [delta (/ v 90)
                    [a b] (rotate [wx wy] (- delta))]
                (recur r x y a b))
              (= action \R)
              (let [delta (/ v 90)
                    [a b] (rotate [wx wy] delta)]
                (recur r x y a b)))
        (+ (Math/abs y) (Math/abs x))))))
