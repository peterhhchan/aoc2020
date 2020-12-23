(ns aoc2020.day23)

(defn build-list [input]
  (loop [res {}
         cur nil
         [f & r] input]
    (if f
      (recur (assoc res f cur) f r)
      {:llist res
       :head (last input)
       :tail (first input)})))

(defn take-cups [n {:keys [llist head tail]}]
  ;; TODO - make this more idiomatic using iterate
  (loop [res     []
         cur     head
         counter 0]
    (if (= n counter)
      res
      (recur (conj res cur)
             (llist cur)
             (inc counter)))))

(defn debug-list [cup-list]
  (vec (take-cups (count (:llist cup-list)) cup-list)))

(defn insert-list [llist pos items]
  (let [cur (llist pos)]
    (-> llist
        (assoc pos (first items))
        (assoc (last items) cur))))

(defn get-tail [cup-list pos]
  (if (nil? (cup-list pos))
    pos
    (get-tail cup-list (cup-list pos))))

(defn step [{:keys [llist head tail] :as cup-list}]
  (let [fs          (take-cups 5 cup-list)
        to-move     (take 3 (drop 1 fs))
        next-head   (last fs)
        destination (->> (concat (range (dec head) 0 -1)
                                 (range (count llist) 0 -1))
                         (remove (set to-move) )
                          first)
        next-list   (-> llist
                       (assoc tail head)
                       (assoc head nil)
                       (insert-list destination to-move))]
    {:llist next-list
     :head  next-head
     :tail  (get-tail next-list head)}))

(def input [1 9 3 4 6 7 2 5 8])
;(def input [3 8 9 1 2 5 4 6 7])
(defn answer-1 [v]
  (->> (concat v v)
       (drop-while #(not= 1 %))
       (drop-while #{1})
       (take (dec (count v)))
       (apply str)))


(defn answer-2 [cup-list]
  (->> (assoc cup-list :head 1)
       (take-cups 3)
       rest
       (reduce *)))

(defn part-1 []
  (let [ll (build-list (reverse input))]
    (->> (iterate step ll)
         (drop 100)
         first
         debug-list
         answer-1)))

;;(723850 655865)
;; ~ 122s
(defn part-2 []
  (let [n         1000000
        mx        (apply max input)
        new-input (vec (concat input (range (inc mx) (inc n))))
        ll        (build-list (reverse new-input))]
    (->> (iterate step ll)
         (drop 10000000)
         first
         answer-2)))
