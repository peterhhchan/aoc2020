(ns aoc.aoc2020.day18
  (:require [clojure.string :as s]))

(defn data []
  (->> (slurp "data/aoc2020_day18.txt")
       s/split-lines))

(defn match-inner [s]
  (re-find #".*?(\([\d]+( [+*] [\d]+)*\))" s))

(defn toLong [s]
  (Long/parseLong s))

(defn find-op [s]
  (re-find #".*?([\d]+ [*+] [\d]+).*" s))

(defn solve-inner-1 [s]
  (cond (re-find #"^[\d]+$" s)
        s
        (find-op s)
        (let [[_ m]     (find-op s)
              [d1 op d2] (s/split m #" ")
              f          (get {"+" + "*" *} op)]
          (solve-inner-1 (s/replace-first s m (str (f (toLong d1) (toLong d2)))))
#_          (solve-inner-1 (s/replace-first s m (str (* (toLong d1) (toLong d2))))))))

(defn solve [f s]
  (if-let [[_ m] (match-inner s)]
    (let [exp (subs m 1 (dec (count m)))]
      (solve f (s/replace s m (f exp))))
    (f s)))

(defn part-1 []
  (->> (data)
       (map (partial solve solve-inner-1))
       (map toLong)
       (reduce +)))

(defn solve-inner-2 [s]
  (cond (re-find #"^[\d]+$" s)
        s
        (re-find #".*?([\d]+ [+] [\d]+).*" s)
        (let [[_ m]     (re-find #".*?([\d]+ [+] [\d]+).*" s)
              [d1 _ d2] (s/split m #" ")]
          (solve-inner-2 (s/replace-first s m (str (+ (toLong d1) (toLong d2))))))

        (re-find #".*?([\d]+ [*] [\d]+).*" s)
        (let [[_ m]     (re-find #".*?([\d]+ [*] [\d]+).*" s)
              [d1 _ d2] (s/split m #" ")]
          (solve-inner-2 (s/replace-first s m (str (* (toLong d1) (toLong d2))))))))

(def input ["1 + 2 * 3 + 4 * 5 + 6"
            "1 + (2 * 3) + (4 * (5 + 6))"
            "2 * 3 + (4 * 5)"
            "5 + (8 * 3 + 9 + 3 * 4 * 3)"
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])

(defn part-2 []
  (->> (data)
       (map (partial solve solve-inner-2))
       (map toLong)
       (reduce +)))
