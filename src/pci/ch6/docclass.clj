(ns pci.ch6.docclass
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str])
  (:import [java.lang Math]))

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

(defn create-fclassifier [get-features]
  {:cc (atom {}), :fc (atom {})
   :get-features get-features
   :minimums (atom {})})

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

(defn weighted-prob [classifier f cat prf &
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
             (weighted-prob classifier f cat fprob)))))

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


(defn set-minimum! [fclassifier cat min]
  (let [mins (fclassifier :minimums)]
    (swap! mins assoc cat min)))

(defn get-minimum [fclassifier cat]
  (let [mins (fclassifier :minimums)]
    (if-let [score (@mins cat)]
      score
      0)))


(defn classify [classifier item &
                {:keys [default] :or {default :unknown}}]
  (loop [cats (categories classifier)
         probs {}
         max 0
         best default]
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

(defn fclassify [fclassifier item &
                 {:keys [default] :or {default :unknown}}]
  (loop [cats (categories fclassifier)
         max 0
         best default]
    (if (seq cats)
      (let [c (first cats)
            p (fisher-prob fclassifier item c)]
        (if (and (> p (get-minimum fclassifier c))
                 (> p max))
          (recur (rest cats) p c)
          (recur (rest cats) max best)))
      best)))


(defn cprob [classifier f cat]
  (let [clf (fprob classifier f cat)]
    (if (zero? clf)
      0
      (let [freq (for [c (categories classifier)]
                   (fprob classifier f c))]
        (/ clf (apply + freq))))))


(defn invchi2 [chi df]
  (let [m (/ chi 2)
        exp (Math/exp (- m))]
    (loop [sum exp, term exp, i 1]
      (if (< i (int (/ df 2)))
        (let [new-term (* term (/ m i))
              new-sum (+ sum new-term)]
          (recur new-sum new-term (inc i)))
        (min sum 1)))))


(defn fisher-prob [classifier item cat]
  (let [get-f (classifier :get-features)
        features (get-f item)
        p (apply *
                 (for [[f _] features]
                   (weighted-prob classifier f cat cprob)))
        fscore (* -2 (Math/log p))]
    (invchi2 fscore (* 2 (count features)))))

