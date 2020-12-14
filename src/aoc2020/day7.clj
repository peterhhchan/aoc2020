(ns aoc2020.day7)

(defn rules []
  {"light red"    {"bright white" 1
                   "muted yellow" 2}
   "dark orange"  {"bright white" 3
                   "muted yellow" 4}
   "bright white" {"shiny gold" 1}
   "muted yellow" {"shiny gold" 2
                   "faded blue" 9}
   "shiny gold"   {"dark olive"   1
                   "vibrant plum" 2}
   "dark olive"   {"faded blue"   3
                   "dotted black" 4}
   "vibrant plum" {"faded blue"   5
                   "dotted black" 6}
   "faded blue"   nil
   "dotted black" nil})

(defn match-rule [s]
  (if (= s "no other bags.")
    nil
    (let [[_ n b] (re-matches #"([0-9]+) ([a-z ]*) bag[s\.]*" s)]
      {b (Integer/parseInt n)})))

(defn rules []
(->> (slurp "data/aoc2020_day7.txt")
     (clojure.string/split-lines)
     (map (fn [l]
            (let [[b rules] (clojure.string/split l #" bags contain ")
                  r           (clojure.string/split rules #", ")]
              {b (->> (map match-rule r)
                      (into {}))})))
     (into {})))

(defn make-rules []
  (->> (rules)
       (map (fn [[k v]]
              {k (set (keys v))}))
       (into {})))

(defn bags
   ;; Recursive - slow
  ([b]
   (bags (make-rules) b))

  ([rules b]
   (let [new-bags (->> rules
                       (filter (fn [[k v]] (get v b)))
                       (map first))]
     (if (seq new-bags)
       (clojure.set/union #{b}  (apply clojure.set/union (map (partial bags rules) new-bags)))
       #{b}))))

(defn bags [b]
  ;; iterative faster
  (loop [rules (make-rules)
         bags  #{b}]
    (let [new-bags (->> rules
                        (filter (fn [[k v]] (seq (clojure.set/intersection v bags))))
                        (map first))]
      (if (seq new-bags)
        (recur (apply dissoc rules new-bags) (into bags new-bags))
        bags))))

(defn part-1 []
  (count (disj (bags "shiny gold") "shiny gold")))

(defn child-bags
  ([b]
   (child-bags (rules) b))

  ([rules b]
   (let [c (get rules b) ]
     (->> c
          (map (fn [[k v]]
                 (+ v
                    (* v (child-bags rules k)))))
          (reduce +)))))

(defn part-2 []
  (child-bags "shiny gold"))
