(ns aoc2020.day21
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
               y (range (count d))]
           (when (seq (compare-foods (d x)(d y) :allergens))
             [(compare-foods (d x)(d y) :allergens)
              (compare-foods (d x)(d y) :foods)]))
         (remove nil?)
         (group-by first)
         (mapv (fn [[k v]]
                {:allergens k
                 :foods (apply clojure.set/intersection (map second v))}))))

(defn part-1 []
  (let [d (->> (data)
               (mapv (fn [l]
                      (let [allergen-list (re-find #"(\(.*\))+" l)
                            foods      (re-seq #"[\w]+ " l)
                            allergens    (re-seq #" [\w]+" (last allergen-list))]
                        {:foods (disj (set foods) "contains ")
                         :allergens (set allergens)}))))
        allergens
        (loop [mm (reduce-foods d)
               mms {}]
          (if (= (count mms) 8)
            mms
            (let [{:keys [allergens foods]}    (->> mm
                                                    (filter #(= 1 (count (:foods %))))
                                                    first)]
              (recur (->> mm
                          (map #(update % :foods disj (first foods)))
                          (map #(update % :allergens disj (first allergens)))
                          (remove #(empty? (:allergens %)))
                          (remove #(empty? (:foods %))))
                     (merge mms
                            {(first allergens) (first foods)})))))
                                        ;      aas (set (vals allergens))
        ]

#_    (prn allergens)
#_    (->> (sort-by first allergens)
         (map second)
         (map str/trim)
         (str/join ","))))

(defn part-2 []
  (let [d (data)]))
