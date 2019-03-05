(ns mtu.wordstats
  (:require [mtu.core :as core]
            [mtu.cljcore :as cljcore]
            [clojure.string :as str])
  )

;;; Assorted NL tools.

;;; Source: http://rosettacode.org/wiki/Levenshtein_distance#Clojure
;;; can be extremely slow eg (levenshtein "restaurant" "restoration")
(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))

;;; lowercase and tokenize a string. Punctuation is removed (except for ').
;;; There are certainly other ways to do tokenization.
;;; \p{L} means match any char of any language.
(defn tokens [s]
  (map str/lower-case 
       (re-seq #"[\p{L}'\d]+" s)))

(defn file-tokens [f]
  (mapcat tokens (cljcore/file-lines f)))

(defn bigrams [tokens]
  (map list tokens (rest tokens)))

(defn remove-stops
  "Remove stop words from a string. Stops is a set of stop words"
  [string stops]
  (str/join
   " "
   (remove #(get stops %) (tokens string))))

(defn text->freq-table [text]
  (-> text
      tokens
      frequencies))

(defn overexpressed [freq base-freq]
  (core/sort-map-by-values
   (reduce (fn [m [k v]]
             (if-let [base (get base-freq k)]
               (assoc m k (/ v base))
               m)) {} freq)))

;;; Data is here: http://norvig.com/ngrams/
(def local-loc "/misc/repos/mtu/data/ngrams/")

;;; Returns a freq map
(defn read-norvig-freqs [file]
  (reduce (fn [map line]
            (let [[word count] (str/split line #"\t")
                 count (Long. count)]
              (assoc map word count)))
          {} (cljcore/file-lines file)))

(core/defn-memoized freq-table [name]
  (read-norvig-freqs (str local-loc name)))


