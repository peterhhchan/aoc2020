(ns aoc2020.day20
  (:require [clojure.string :as s]))

(defn data []
  (-> (slurp "data/aoc2020_day20.txt")
       (s/split #"\n\n")
))

(defn toInt [s]
  (Integer/parseInt s))

(defn toLong [s]
  (Long/parseLong s))

(defn side [s]
  (-> (apply str (map #({\. "0" \# "1"} %) s))
      (Integer/parseInt 2)))


(defn ids [tile]
  (let [t1   (side (first tile))
        t2   (side (reverse (first tile)))
        r1 (side (first (rotate-picture tile)))
        r2 (side (reverse (first (rotate-picture tile))))
        b1   (side (first (rotate-picture (rotate-picture tile))))
        b2  (side (reverse (first (rotate-picture (rotate-picture tile)))))
        l1  (side (first (rotate-picture (rotate-picture (rotate-picture tile)))))
        l2  (side (reverse (first (rotate-picture (rotate-picture (rotate-picture tile))))))]
    {:all #{t1 t2 l1 l2 r1 r2 b1 b2}}))

(defn part-1 []
  (let [d (data)
        all-tiles (->> d
                   (map s/split-lines)
                   (map (fn [tile]
                          (let [[_ id] (re-find #"Tile ([\d]+)\:" (first tile))
                                t (mapv vec (rest tile))]
                            (merge {:id (toInt id)
                                    :tile t}
                                   (ids t)))))
                   (into #{}))
       cxs (->> (frequencies (flatten ( map seq (map :all all-tiles))))
                (filter #(= 2 (second %)))
                (map first)
                (into #{}))]

    (->> all-tiles
         (filter (fn [t] (= 4  (count (clojure.set/intersection cxs (:all t))))))
         (map :id)
         (reduce *))))

(defn create-pic [{:keys [tile] :as pic}]
  (let [t (side (first tile))
        b (side (last tile))

        r (side (map last tile))
        l (side (map first tile))]
    (merge pic
           {:top   t
            :right  r
            :bottom b
            :left l})))

(defn remove-tile [tiles t]
  (set (remove #(= (:id %) (:id t)) tiles)))

(defn part-2 []
  (let [d (data)
        all-tiles (->> d
                   (map s/split-lines)
                   (map (fn [tile]
                          (let [[_ id] (re-find #"Tile ([\d]+)\:" (first tile))
                                t (mapv vec (rest tile))]
                            (merge {:id (toInt id)
                                    :tile t}
                                   (ids t)))))
                   (into #{}))
        freqs (frequencies (apply concat (map seq (map :all all-tiles))))
        cxs   (->> (filter #(= 2 (second %)) freqs)
                   (map first)
                   (into #{}))

        start-tile
        (->> all-tiles
             (filter (fn [t] (= 4  (count (clojure.set/intersection cxs (:all t))))))
             (first)
             (rotations)
             (filter  #(cxs (:top %)))
             (filter  #(cxs (:left %)))
             first)]

    (loop [tiles (remove-tile all-tiles start-tile)

           grid  {[0 0] start-tile}
           top  (:top start-tile)
           left (:left start-tile)
           x 0
           y 0]
      (if (= x y 11)
        grid
        (if (< y 11)
          (let [m        (first (filter #(get (:all %) top) tiles))
                next-tile      (->> (rotations m)
                                    (filter #(= (:bottom %) top))
                                    first)]

            (if next-tile
              (recur (remove-tile tiles next-tile)
                     (merge grid {[x (inc y)] next-tile})
                     (:top next-tile) left
                     x (inc y))))
          (let [m   (first (filter #(get (:all %) left) tiles))
                next-tile (->> (rotations m)
                               (filter #(= (:right  %) left))
                               first)]
            (if next-tile
              (recur (remove-tile tiles next-tile)
                     (merge grid {[(inc x) 0] next-tile})
                     (:top next-tile) (:left next-tile)
                     (inc x) 0)
                  grid)))))))

(defn grid-xy []
  (for [y (range 12)]
    (for [x (range 12)]
      [(- 11 x) (- 11 y)])))

(defn part-2b []
  (let [grid (part-2)]
    (->> (grid-xy)
         (mapv (partial mapv (fn [xy]
#_                               (:tile (get grid xy))
                               (strip-border (:tile (get grid xy)))))))))

(defn  rotate-pic [{:keys [tile] :as obj}]
  (create-pic  (assoc obj :tile  (apply map vector (reverse tile)))))

(defn  transform-pic [{:keys [tile] :as obj}]
  (create-pic  (assoc obj :tile (apply map vector tile))))


(defn rotations [pic]
  (let [rots (->> (iterate rotate-pic pic)
                  (drop 1)
                  (take 4))]
    (->> (map transform-pic rots)
         (concat rots))))

(defn strip-border [t]
  (->> (butlast (rest t))
       (mapv (comp vec butlast rest))))



(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn giant-grid [g]
  (->> (map (fn [ms] (apply map concat ms)) g)
       (apply concat))
)
