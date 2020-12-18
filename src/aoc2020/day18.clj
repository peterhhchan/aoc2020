(ns aoc2020.day18
  (:require [clojure.string :as s]
            [clojure.string :as str]))

(defn data []
  (->> (slurp "data/aoc2020_day18.txt")
       s/split-lines))

(defn toLong [s]
  (Long/parseLong s))

(defn find-op [s]
  (re-find  #".*?([0-9]+ [+\*] [0-9]+).*" s))

(defn find-parens [s]
  (re-find #".*?(\([0-9]+\)).*" s))

(defn eval-op [s]
  (let [[d1 op d2] (s/split s #" ")]
    (if (= op "+")
      (+
       (toLong d1)
       (toLong d2))
      (*
       (toLong d1)
       (toLong d2)))))

(defn my-eval [s]
  (cond (find-parens s)
        (let [[a m]  (find-parens s)]
          (my-eval (str/replace-first s m (subs m 1 (- (count m) 1)))))

        (find-op s)
        (let [[_ m]  (find-op s)]
          (my-eval (str/replace-first s m (str (eval-op m)))))

        :else
        (toLong s)))

(defn part-1 []
  (->> (data)
       (map my-eval )
       (reduce +)))


(defn find-add [s]
  (re-find  #".*?([0-9]+ [\+] [0-9]+).*" s))

(defn find-mult* [s]
  (re-find  #".*?(\([0-9]+( \* [0-9]+)+\)).*" s))

(defn find-mult [s]
  (re-find  #".*?([0-9]+ [\*] [0-9]+).*" s))


(defn my-eval-2 [s]
  (cond (find-parens s)
        (let [[a m]  (find-parens s)]
          (my-eval-2 (str/replace-first s m (subs m 1 (- (count m) 1)))))

        (find-mult* s)
        (let [[_ m _]  (find-mult* s)
              nv     (->> (str/split (subs m 1 (dec (count m))) #" ")
                          (remove #(= "*" %))
                          (map toLong)
                          (apply *)
                          (str))]
          (my-eval-2 (str/replace-first s m nv)))

        (find-add s)
        (let [[_ m]  (find-add s)]
          (my-eval-2 (str/replace-first s m (str (eval-op m)))))

        (find-mult s)
        (let [[_ m]  (find-mult s)]
          (my-eval-2 (str/replace-first s m (str (eval-op m)))))

        :else
        (toLong s)))

(defn part-2 []
  (->> (data)
       (map my-eval-2 )
       (reduce +)))


(def input ["1 + 2 * 3 + 4 * 5 + 6"
            "1 + (2 * 3) + (4 * (5 + 6))"
            "2 * 3 + (4 * 5)"
            "5 + (8 * 3 + 9 + 3 * 4 * 3)"
            "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
            "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])
