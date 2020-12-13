(ns aoc2020.day2)

(defn valid? [l]
  (let [[policy pw] (clojure.string/split l #":")
        [f pw-char] (clojure.string/split policy #" ")
        [mn mx]     (clojure.string/split f #"-")
        mn*         (Integer/parseInt mn)
        mx*         (Integer/parseInt mx)
        o           (get (frequencies pw) (first pw-char) 0)]
    ;;(or >= mn* o mx*)
    (and
         (>= o mn*)
         (<= o mx*))))

(defn valid2? [l]
  (let [[policy pw] (clojure.string/split l #":")
        [f pw-char] (clojure.string/split policy #" ")
        [mn mx]     (clojure.string/split f #"-")
        mn*         (Integer/parseInt mn)
        mx*         (Integer/parseInt mx)
        a           (= (get pw mn* nil) (first pw-char))
        b           (= (get pw mx* nil) (first pw-char))]
    (or (and a (not b))
        (and b (not a)))))


(defn day2 []
  (->> (slurp "data/aoc2020_day2.txt")
       clojure.string/split-lines
       (filter valid2?)
       count))
