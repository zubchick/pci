(ns pci.ch1.movies
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [pci.ch1.recommendations :as recommendations]))


(defn load-movie-lens [path]
  (let [movies (with-open [rdr (io/reader (str path "/u.item"))]
                 (into {}
                       (for [line (line-seq rdr)]
                         (subvec (str/split line #"\|") 0 2))))]
    (with-open [rdr (io/reader (str path "/u.data"))]
      (apply merge-with merge
             (for [line (line-seq rdr)
                   :let [[user movieid rating ts] (str/split line #"\t")]]
               {user {(movies movieid) (Float/parseFloat rating)}})))))

(def prefs (load-movie-lens "/Users/zubchick/workspace/pci/src/pci/ch1/data"))
