(ns pci.ch1.recommendations
  (:require clojure.set
            [clojure.math.numeric-tower :as math]))

(def critics
  {"Lisa Rose" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.5
                 "Just My Luck" 3.0, "Superman Returns" 3.5
                 "You, Me and Dupree" 2.5, "The Night Listener" 3.0}
   "Gene Seymour" {"Lady in the Water" 3.0, "Snakes on a Plane" 3.5
                    "Just My Luck" 1.5, "Superman Returns" 5.0
                    "The Night Listener" 3.0, "You, Me and Dupree" 3.5}
   "Michael Phillips" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.0
                        "Superman Returns" 3.5, "The Night Listener" 4.0}
   "Claudia Puig" {"Snakes on a Plane" 3.5, "Just My Luck" 3.0
                    "The Night Listener" 4.5, "Superman Returns" 4.0
                    "You, Me and Dupree" 2.5}
   "Mick LaSalle" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0
                    "Just My Luck" 2.0, "Superman Returns" 3.0
                    "The Night Listener" 3.0, "You, Me and Dupree" 2.0}
   "Jack Matthews" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0
                     "The Night Listener" 3.0, "Superman Returns" 5.0
                     "You, Me and Dupree" 3.5},
   "Toby" {"Snakes on a Plane" 4.5, "You, Me and Dupree" 1.0
            "Superman Returns" 4.0}})


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
    (if (count (clojure.set/intersection (set keys-pref1)
                                         (set keys-pref2)))
      (let [sum-of-sqr (apply + (for [item keys-pref1 :when (pref2 item)]
                                  (sum-of-squares (pref1 item) (pref2 item))))]
        (/ 1 (+ 1 sum-of-sqr)))
      0)))

(defn sim-pearson [prefs person1 person2]
  (let[pref1 (prefs person1)
       pref2 (prefs person2)
       keys-pref1 (keys pref1)
       keys-pref2 (keys pref2)
       ;; общие интересы
       si (clojure.set/intersection (set keys-pref1)
                                    (set keys-pref2))
       n (count si)]

    (if (= 0 n)
      0
      (let [big-table (for [item si] (let [i1 (pref1 item)
                                           i2 (pref2 item)]
                                       [i1, i2
                                        (math/expt i1 2), (math/expt i2 2)
                                        (* i1 i2)]))
            zip-table (apply zip big-table)
            sums-table (map #(apply + %) zip-table)
            [sum1 sum2 sum1sq sum2sq psum] sums-table
            num (- psum (* sum1 (/ sum2 n)))
            den (math/sqrt (* (- sum1sq (/ (math/expt sum1 2) n))
                              (- sum2sq (/ (math/expt sum2 2) n))))]
        (if (= 0 den)
          0
          (/ num den))))))
