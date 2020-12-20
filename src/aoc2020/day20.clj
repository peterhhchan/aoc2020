(ns aoc2020.day20
  (:require [clojure.string :as s]))

(defn images []
  (-> (slurp "data/aoc2020_day20.txt")
      (s/split #"\n\n")))

(defn toInt [s]
  (Integer/parseInt s))

(defn border-id [s]
  (-> (apply str (map #({\. "0" \# "1"} %) s))
      (Integer/parseInt 2)))

(defn rotate-grid [g] (apply mapv vector (reverse g)))
(defn transpose-grid [g] (apply mapv vector g))

(defn all-rotations [g]
  (let [rots  (->> (iterate rotate-grid g)
                   (take 4))]
    (concat rots (map transpose-grid rots))))

(defn borders [image]
  {:top     (border-id (first image))
   :bottom  (border-id (last image))
   :right   (border-id (map last image))
   :left    (border-id (map first image))})

(defn all-sides [image]
  (->> (all-rotations image)
       (map borders)
       (mapcat (juxt :top :bottom :right :left))
       set))

(defn all-neighboring-edges [tiles]
  (->> tiles
       (mapcat (comp seq :edges))
       frequencies
       (filter #(= 2 (second %)))
       (map first)
       (set)))

(defn all-tiles []
(->> (images)
     (map s/split-lines)
     (map (fn [[header & img]]
            {:id    (toInt (second (re-find #"Tile ([\d]+)\:" header)))
             :image (mapv vec img)
             :edges (all-sides (mapv vec img))}))
     (into #{})))

(defn corners [tiles neighbors]
   (->> tiles
        (filter #(= 4 (count (clojure.set/intersection neighbors (:edges %)))))))

(defn part-1 []
  (let [all-tiles     (all-tiles)
        all-neighbors (all-neighboring-edges all-tiles)]
    (->> (corners  all-tiles all-neighbors)
         (map :id)
         (reduce *))))

(defn all-images [{:keys [id image edges]}]
  (->> (all-rotations image)
       (map #(merge {:id id
                     :edges edges
                     :image %}
                    (borders %)))))

(defn remove-tile [tiles t]
  (set (remove #(= (:id %) (:id t)) tiles)))

(defn solve-image [all-tiles all-neighbors start-tile]
  (loop [tiles (remove-tile all-tiles start-tile)
         grid  {[0 0] start-tile}
         top   (:top start-tile)
         left  (:left start-tile)
         x     0
         y     0]
      (if (= x y 11)
        grid
        (if (< y 11)
          (let [m         (first (filter #(get (:edges %) top) tiles))
                next-tile (->> (all-images m)
                               (filter #(= (:bottom %) top))
                               first)]
            (if next-tile
              (recur (remove-tile tiles next-tile)
                     (merge grid {[x (inc y)] next-tile})
                     (:top next-tile) left
                     x (inc y))))
          (let [m         (first (filter #(get (:edges %) left) tiles))
                next-tile (->> (all-images m)
                               (filter #(= (:right  %) left))
                               first)]
            (if next-tile
              (recur (remove-tile tiles next-tile)
                     (merge grid {[(inc x) 0] next-tile})
                     (:top next-tile) (:left next-tile)
                     (inc x) 0)
                  grid))))))


(defn stich-images [g]
  (->>  (for [y (range 12)]
          (for [x (range 12)]
            [(- 11 x) (- 11 y)]))
        (mapv (partial mapv #(strip-border (:image (get g %)))))
        (mapv (fn [ms] (apply map concat ms)))
        (apply concat)
        (mapv vec)))

(defn strip-border [t]
  (->> (butlast (rest t))
       (mapv (comp vec butlast rest))))

(def monster
  ["                  # "
   "#    ##    ##    ###"
   " #  #  #  #  #  #   "])

(defn offsets [monster]
  (->>   (for [x (range (count (first monster)))
               y (range (count monster))]
           (when (= \# (get-in monster [y x]))
             [x y]))
         (remove nil?)))

(defn monster-offsets []
  (map offsets (all-rotations monster)))

(defn base-roughness [picture]
  (->> (for [x (range (count (first picture)))
             y (range (count (first picture)))]
         (when (= \# (get-in picture [x y]))
           [x y]))
       (remove nil?)
       (into #{})))

(defn part-2 []
  (let [tiles         (all-tiles)
        all-neighbors (all-neighboring-edges tiles)
        start-tile    (->> (corners tiles all-neighbors)
                              first
                              (all-images)
                              (filter #(all-neighbors (:left %)))
                              (filter #(all-neighbors (:top %)))
                              ;; There are two possible starting positions for the corner
                              first)
        image         (solve-image tiles all-neighbors start-tile)
        picture       (stich-images image)
        roughness     (base-roughness picture)
        monsters      (monster-offsets)]
    (->>  (for [x (range (count (first picture)))
                y (range (count picture))]
            (->> monsters
                 (map (fn [monster]
                        (let [pos (map (fn [[a b]] [(+ a x) (+ b y)]) monster)]
                           (when (every? #(= \# (get-in picture [(first %) (second %)])) pos)
                             (into #{} pos)))))
                 (remove empty?)
                 (remove nil?)
                 (apply clojure.set/union)))
          (remove empty?)
          (apply clojure.set/union)
          count
          (- (count roughness)))))
