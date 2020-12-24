(ns aoc.aoc2020.day4
  (:require [clojure.string :as str]))

(def fields #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})


(defn part-1 []
  (let [passports (-> (slurp "data/aoc.aoc2020.day4.txt")
                      (str/split #"\n\n"))]
    (->> passports
         (map (fn [p]
                (let [fields (->> (str/split-lines p)
                                  (mapcat #(str/split % #" "))
                                  (map #(str/split % #":"))
                                  (map first)
                                  (into #{}))]
                  (count (disj fields "cid")))))
         (filter #(= 7 %)))))

(defn year? [s]
  (when (re-matches #"[0-9]{4}" s)
    (Integer/parseInt s)))

(defn height? [s]
  (when-let [h (re-matches #"([0-9]{2,3})(cm|in)" s)]
    (or (and (= "in" (last h))
             (<= 59 (Integer/parseInt (second h)) 76))
        (and (= "cm" (last h))
             (<= 150 (Integer/parseInt (second h)) 193)))))

(defn hair? [s]
  (re-matches #"#[0-9,a-f]{6}" s))

(defn eye? [s]
  (get #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s))

(defn pid? [s]
  (re-matches #"[0-9]{9}" s))

(defn valid? [[k v]]
  (case k
       "byr" (<= 1920 (year? v) 2002)
       "iyr" (<= 2010 (year? v) 2020)
       "eyr" (<= 2020 (year? v) 2030)
       "hgt" (height? v)
       "hcl" (hair? v)
       "ecl" (eye? v)
       "pid" (pid? v)
       "cid" true))

(defn part-2 []
  (let [passports (-> (slurp "data/aoc2020_day4.txt")
                      (str/split #"\n\n"))]
    (->> passports
         (map (fn [p]
                (let [fields (->> (str/split-lines p)
                                  (mapcat #(str/split % #" "))
                                  (map #(str/split % #":"))
                                  (filter valid?)
                                  (map first)
                                  (into #{}))]
                  (count (disj fields "cid")))))
         (filter #(= 7 %)))))
