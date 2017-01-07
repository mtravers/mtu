(ns mtu.wordstats
  (:use mtu.core)
  (:use mtu.cljcore)
  )

(defn overexpressed [freq base-freq]
  (sort-map-by-values
   (reduce (fn [m [k v]]
             (if-let [base (get base-freq k)]
               (assoc m k (/ v base))
               m)) {} freq)))

;;; Data is here: http://norvig.com/ngrams/
(def local-loc "data/ngrams/")

;;; Returns a freq map
(defn read-norvig-freqs [file]
  (reduce (fn [map line]
            (let [[word count] (clojure.string/split line #"\t")
                 count (Long. count)]
              (assoc map word count)))
          {} (file-lines file)))

(defn-memoized freq-table [name]
  (read-norvig-freqs (str local-loc name)))
