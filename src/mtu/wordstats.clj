(ns mtu.wordstats
  (:require [mtu.core :as core]
            [mtu.cljcore :as cljcore]
            [clojure.string :as str])
;  (:use mtu.cljcore)
  )

(defn overexpressed [freq base-freq]
  (core/sort-map-by-values
   (reduce (fn [m [k v]]
             (if-let [base (get base-freq k)]
               (assoc m k (/ v base))
               m)) {} freq)))

;;; Data is here: http://norvig.com/ngrams/
(def local-loc "data/ngrams/")

;;; Returns a freq map
(defn read-norvig-freqs [file]
  (reduce (fn [map line]
            (let [[word count] (str/split line #"\t")
                 count (Long. count)]
              (assoc map word count)))
          {} (cljcore/file-lines file)))

(core/defn-memoized freq-table [name]
  (read-norvig-freqs (str local-loc name)))
