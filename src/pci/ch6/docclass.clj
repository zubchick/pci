(ns pci.ch6.docclass
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(defn get-words [doc]
  (let [words (for [word (str/split doc #"\W")
                    :let [str-count (count word)]
                    :when (and (> str-count 2)
                               (< str-count 20))]
                [(.toLowerCase word) 1])]
    (into {} words)))

(defn create-classifier [get-features]
  {:cc (atom {}), :fc (atom {}) :get-features get-features})

(defn- fc-inc [fc f cat]
  (if-let [pairs (fc f)]
    (if-let [category (pairs cat)]
      (assoc-in fc [f cat] (inc category))
      (merge-with merge fc {f {cat 1}}))
    (assoc fc f {cat 1})))

(defn classifier-incf [classifier f cat]
  (let [fc (classifier :fc)]
    (swap! fc fc-inc f cat)))

(defn classifier-incc [classifier cat]
  (let [cc (classifier :cc)]
    (if-let [category ((deref cc) cat)]
      (swap! cc assoc cat (inc category))
      (swap! cc assoc cat 1))))

(defn classifier-fcount [classifier f cat]
  (let [fc (classifier :fc)]
    (if-let [category ((deref fc) f)]
      (if-let [value (category cat)]
        value
        0)
      0)))

(defn classifier-cat-count [classifier cat]
  (if-let [res ((deref (classifier :cc)) cat)]
    res
    0))

(defn classifier-total-count [classifier]
  (apply + (-> classifier :cc deref vals)))

(defn classifier-categories [classifier]
  (-> classifier :cc deref keys))


(defn train [classifier item cat]
  (let [get-f (classifier :get-features)
        features (get-f item)]
    (doseq [[f _] features]
      (classifier-incf classifier f cat)))
  (classifier-incc classifier cat))

(defn sampl-train [classifier]
  (let [cl classifier]
    (train cl "Nobody owns the water." :good)
    (train cl "the quick rabbit jumps fences" :good)
    (train cl "buy pharmaceuticals now" :bad)
    (train cl "make quick money at the online casino" :bad)
    (train cl "the quick brown fox jumps" :good)))

(defn classifier-fprob [classifier f cat]
  (let [count (classifier-catcount classifier cat)]
    (if (not= count 0)
      (/ (classifier-fcount classifier f cat)
         count)
      0)))

(defn classifier-weight-prob [classifier f cat prf &
                              {:keys [weight ap] :or {weight 1 ap 1/2}}]
  (let [basic-prob (prf f cat)
        totals (apply + (for [c (classifier-categories classifier)]
                          (classifier-fcount f c)))]
    (/ (+ (* weight ap) (* totals basic-prob))
       (+ weight totals))))

