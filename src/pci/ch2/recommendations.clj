(ns pci.ch2.recommendations
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


(defn transform-prefs [prefs]
  (apply merge-with merge
         (for [[person pprefs] prefs
               [film rate] pprefs]
           {film {person rate}})))


(def movies (transform-prefs critics))


(defn sum-of-squares [x y]
  (math/expt (- x y) 2))


(defn zip [& colls]
  (apply map vector colls))


(defn sim-distance [prefs person1 person2]
  (let [pref1 (prefs person1)
        pref2 (prefs person2)
        keys-pref1 (keys pref1)
        keys-pref2 (keys pref2)]
    ;; common interests
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
        ;; common interests
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
        (if (zero? den)
          0
          (/ num den))))))


(defn top-matches [prefs person &
                   {:keys [n similarity] :or {n 5, similarity sim-pearson}}]
  (let [scores (for [other (keys prefs) :when (not= other person)]
                 [(similarity prefs person other), other])
        result (-> (sort-by first scores) reverse vec)
        len (count result)
        end (if (> n len) len n)]
    (subvec result 0 end)))


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

        concated (apply concat totals-and-sums)
        totals (group-sum (map first concated))
        sim-sums (group-sum (map second concated))
        rankings (for [[item, total] totals] [(/ total (sim-sums item)), item])]
    (-> rankings sort reverse)))


(defn calculate-similar-items [prefs &
                               {:keys [n] :or {n 10}}]
  (let [item-prefs (transform-prefs prefs)]
    (apply merge (for [item (keys item-prefs)]
                   {item (top-matches item-prefs, item, :n n
                                      :similarity sim-distance)}))))


(def item-sim (calculate-similar-items critics))


(defn get-recommended-items [prefs item-match user]
  (let [pprefs (prefs user)
        pprefs-keys (set (keys pprefs))
        prepared (apply concat
                        (for [[item rating] pprefs]
                          (for [[similarity item2] (item-match item)
                                :when (not (pprefs-keys item2))]
                            [[item2 (* similarity rating)]
                             [item2 similarity]])))

        scores (group-sum (map first prepared))
        total-sim (group-sum (map second prepared))
        rankings (for [[item, score] scores]
                   (if (zero? (total-sim item))
                     [0 item]
                     [(/ score (total-sim item)) item]))]
    (-> rankings sort reverse)))
