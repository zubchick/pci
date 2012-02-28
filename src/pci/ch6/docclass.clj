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
  {:cc (atom {}), :fc (atom {})
   :get-features get-features
   :thresholds (atom {})})

(defn- fc-inc [fc f cat]
  (if-let [pairs (fc f)]
    (if-let [category (pairs cat)]
      (assoc-in fc [f cat] (inc category))
      (merge-with merge fc {f {cat 1}}))
    (assoc fc f {cat 1})))

(defn incf! [classifier f cat]
  (let [fc (classifier :fc)]
    (swap! fc fc-inc f cat)))

(defn incc! [classifier cat]
  (let [cc (classifier :cc)]
    (if-let [category ((deref cc) cat)]
      (swap! cc assoc cat (inc category))
      (swap! cc assoc cat 1))))

(defn fcount [classifier f cat]
  (let [fc (classifier :fc)]
    (if-let [category ((deref fc) f)]
      (if-let [value (category cat)]
        value
        0)
      0)))

(defn cat-count [classifier cat]
  (if-let [res ((deref (classifier :cc)) cat)]
    res
    0))

(defn total-count [classifier]
  (apply + (-> classifier :cc deref vals)))

(defn categories [classifier]
  (-> classifier :cc deref keys))


(defn train! [classifier item cat]
  (let [get-f (classifier :get-features)
        features (get-f item)]
    (doseq [[f _] features]
      (incf! classifier f cat)))
  (incc! classifier cat)
  nil)

(defn sampl-train! [classifier]
  (let [cl classifier]
    (train! cl "Nobody owns the water." :good)
    (train! cl "the quick rabbit jumps fences" :good)
    (train! cl "buy pharmaceuticals now" :bad)
    (train! cl "make quick money at the online casino" :bad)
    (train! cl "the quick brown fox jumps" :good)))

(defn fprob [classifier f cat]
  (let [count (cat-count classifier cat)]
    (if (not= count 0)
      (/ (fcount classifier f cat)
         count)
      0)))

(defn weight-prob [classifier f cat prf &
                              {:keys [weight ap] :or {weight 1 ap 1/2}}]
  (let [basic-prob (prf classifier f cat)
        totals (apply + (for [c (categories classifier)]
                          (fcount classifier f c)))]
    (/ (+ (* weight ap) (* totals basic-prob))
       (+ weight totals))))

(defn docprob [classifier item cat]
  (let [get-f (classifier :get-features)
        features (get-f item)]
    (apply *
           (for [[f _] features]
             (weight-prob classifier f cat fprob)))))

(defn prob [classifier item cat]
  (let [catprob (/ (cat-count classifier cat)
                   (total-count classifier))
        docprob (docprob classifier item cat)]
    (* catprob docprob)))

(defn set-threshold! [classifier cat t]
  (let [thresholds (classifier :thresholds)]
    (swap! thresholds assoc cat t)))

(defn get-threshold [classifier cat]
  (let [thresholds (classifier :thresholds)]
    (if-let [score (@thresholds cat)]
      score
      1)))

(defn classify [classifier item &
                {:keys [default] :or {default :unknown}}]
  (loop [cats (categories classifier)
         probs {}
         max 0
         best nil]
    (if (seq cats)
      (let [cat (first cats)
            candidate (prob classifier item cat)
            new-prob (assoc probs cat candidate)]
        (if (> candidate max)
          (recur (rest cats) new-prob candidate cat)
          (recur (rest cats) new-prob  max best)))
      (loop [sq probs]
        (if (seq sq)
          (let [cat (ffirst sq)]
            (if (= cat best)
              (recur (rest (seq sq)))
              (if (> (* (probs cat) (get-threshold classifier best))
                     (probs best))
                default
                (recur (rest (seq sq))))))
          best)))))