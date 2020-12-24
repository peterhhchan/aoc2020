(ns aoc.aoc2020.day21
  (:require [clojure.string :as s]
            [clojure.string :as str]))

(defn data []
  (->> (slurp "data/aoc2020_day21.txt")
       s/split-lines))

#_(defn data []
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
   "trh fvjkl sbzzf mxmxvkd (contains dairy)"
   "sqjhc fvjkl (contains soy)"
   "sqjhc mxmxvkd sbzzf (contains fish)"])

(defn compare-foods [f1 f2 k]
  (clojure.set/intersection (k f1) (k f2)))

(defn reduce-foods [d]
    (->> (for [x (range (count d))
               y (range (count d))
               :let [allergens (compare-foods (d x)(d y) :allergens)]
               :when (seq allergens)]
           [allergens (compare-foods (d x)(d y) :foods)])
         (group-by first)
         (mapv (fn [[k v]]
                {:allergens k
                 :foods (apply clojure.set/intersection (map second v))}))))

(defn find-allergens [ingredients]
  (let [num-allergens (->> ingredients
                           (map :allergens)
                           (apply clojure.set/union)
                           count) ]
    (loop [mm             (reduce-foods ingredients)
           allergen->food {}]
      (if (= (count allergen->food) num-allergens)
        allergen->food
        (let [{:keys [allergens foods]} (->> mm
                                             (filter #(= 1 (count (:foods %))))
                                             first)]
          (recur (->> mm
                      (map #(update % :foods disj (first foods)))
                      (map #(update % :allergens disj (first allergens)))
                      (remove #(empty? (:allergens %)))
                      (remove #(empty? (:foods %))))
                 (merge allergen->food
                        {(first allergens) (first foods)})))))))

(defn ingredients []
  (->> (data)
       (mapv (fn [l]
               (let [allergen-list (re-find #"(\(.*\))+" l)
                     foods      (re-seq #"[\w]+ " l)
                     allergens    (re-seq #" [\w]+" (last allergen-list))]
                 {:foods (disj (set foods) "contains ")
                  :allergens (set allergens)})))))

(defn part-1 []
  (let [ingredients (ingredients)
        allergens (-> (find-allergens ingredients)
                      vals
                      set)]
    (->> ingredients
         (mapcat :foods)
         (remove allergens)
         count)))

(defn part-2 []
(->>  (ingredients)
      (find-allergens)
      (sort-by first)
      (map second)
      (map str/trim)
      (str/join ",")))

;; Method 2 - way cleaner
(defn part-1 []
  (let [ingredients (ingredients)
        allergens   (->> ingredients
                         (mapcat :allergens)
                         (into #{}))
        dangerous     (->> allergens
                           (map (fn [a]
                                  (->> ingredients
                                       (filter #((:allergens %) a))
                                       (map :foods)
                                       (apply clojure.set/intersection))))
                           (apply clojure.set/union))]
    (->> ingredients
         (mapcat :foods)
         (remove dangerous)
         count)))
