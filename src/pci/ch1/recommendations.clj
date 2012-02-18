(ns pci.ch1.recommendations
  (:require clojure.set
            [clojure.math.numeric-tower :as math]))

(def critics
  {"Lisa Rose" {"Lady in the Water" 5/2, "Snakes on a Plane" 7/2
                "Just My Luck" 3, "Superman Returns" 7/2
                "You, Me and Dupree" 5/2, "The Night Listener" 3}
   "Gene Seymour" {"Lady in the Water" 3, "Snakes on a Plane" 7/2
                   "Just My Luck" 3/2, "Superman Returns" 5
                   "The Night Listener" 3, "You, Me and Dupree" 7/2}
   "Michael Phillips" {"Lady in the Water" 5/2, "Snakes on a Plane" 3
                       "Superman Returns" 7/2, "The Night Listener" 4}
   "Claudia Puig" {"Snakes on a Plane" 7/2, "Just My Luck" 3
                   "The Night Listener" 9/2, "Superman Returns" 4
                   "You, Me and Dupree" 5/2}
   "Mick LaSalle" {"Lady in the Water" 3, "Snakes on a Plane" 4
                   "Just My Luck" 2, "Superman Returns" 3
                   "The Night Listener" 3, "You, Me and Dupree" 2}
   "Jack Matthews" {"Lady in the Water" 3, "Snakes on a Plane" 4
                    "The Night Listener" 3, "Superman Returns" 5
                    "You, Me and Dupree" 7/2},
   "Toby" {"Snakes on a Plane" 9/2, "You, Me and Dupree" 1
           "Superman Returns" 4}})

(defn- map-get [mp key default]
  (or (mp key) default))


(defn- transform-person-prefs [pprefs person]
  (reduce merge {}
          (map (fn [[film rate]] {film {person rate}})
               pprefs)))


(defn transform-prefs [prefs]
  (reduce (fn [result [person pprefs]]
            (merge-with merge result
                        (transform-person-prefs pprefs person)))
          {}
          prefs))

(defn sum-of-squares [x y]
  (math/expt (- x y) 2))

(defn zip [& colls]
  (apply map vector colls))

(defn sim-distance [prefs person1 person2]
  (let [pref1 (prefs person1)
        pref2 (prefs person2)
        keys-pref1 (keys pref1)
        keys-pref2 (keys pref2)]
    ;; если есть общие интересы
    (if (seq (clojure.set/intersection (set keys-pref1)
                                       (set keys-pref2)))
      (let [sum-of-sqr (apply + (for [item keys-pref1 :when (pref2 item)]
                                  (sum-of-squares (pref1 item) (pref2 item))))]
        (/ 1 (+ 1 sum-of-sqr)))
      0)))

(defn sim-pearson [prefs person1 person2]
  (let [pref1 (prefs person1)
        pref2 (prefs person2)
        keys-pref1 (keys pref1)
        keys-pref2 (keys pref2)
        ;; общие интересы
        si (clojure.set/intersection (set keys-pref1)
                                     (set keys-pref2))
        n (count si)]

    (if (= 0 n)
      0
      (let [big-table (for [item si :let [i1 (pref1 item)
                                          i2 (pref2 item)]]
                        [i1, i2, (math/expt i1 2), (math/expt i2 2), (* i1 i2)])
            zip-table (apply zip big-table)
            sums-table (map #(apply + %) zip-table)
            [sum1 sum2 sum1sq sum2sq psum] sums-table
            num (- psum (* sum1 (/ sum2 n)))
            den (math/sqrt (* (- sum1sq (/ (math/expt sum1 2) n))
                              (- sum2sq (/ (math/expt sum2 2) n))))]
        (if (= 0 den)
          0
          (/ num den))))))

(defn top-matches [prefs person &
                   {:keys [n similarity] :or {n 5, similarity sim-pearson}}]
  (let [scores (for [other (keys prefs) :when (not= other person)]
                 [(similarity prefs person other) other])]
    (-> (sort-by first scores) reverse vec (subvec 0 n))))


(defn group-sum
  "(group-sum [[:a 1] [:b 2] [:a 1] [:b 2]])
   {:b 4, :a 2}"
  [kv-seq]
  (reduce (fn [result item]
            (let [[key value] item
                  total-sum (or (result key) 0)]
              (assoc result key (+ total-sum value))))
          {}
          kv-seq))

(defn get-recommendations [prefs person &
                           {:keys [similarity] :or {similarity sim-pearson}}]
  (let [totals-and-sums
        (for [other (keys prefs) :when (not= other person)
              :let [sim (similarity prefs person other)]
              :when (> sim 0)]
          (for [item (keys (prefs other)) :let [rating ((prefs person) item)]
                :when (or (nil? rating)
                          (zero? rating))]
            [[item, (* sim ((prefs other) item))]
             [item, sim]]))

        prepare (fn [getter] (reduce concat []
                                    (map #(map getter %) totals-and-sums)))
        totals (group-sum (prepare first))
        sim-sums (group-sum (prepare second))
        rankings (for [[item, total] totals] [(/ total (sim-sums item)), item])]
    (-> rankings sort reverse)))
