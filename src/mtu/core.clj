(ns mtu.core
  "Various generally useful utilities to keep mt sane"
  (:require [clojure.string :as str])
  (:require clojure.set)
  (:require clojure.java.shell)
  )

;;; (many based on CL; see https://github.com/mtravers/mtlisp/blob/master/mt-utils.lisp )

;;; ⩇⩆⩇ Memoization ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See https://github.com/clojure/core.memoize/ for more memoize hacks
(defmacro defn-memoized "Like `defn`, but produces a memoized function"
  [name args & body]
  ;; This crock is because you can't have multiple varadic arg signatures...sigh
  (if (string? args)
    `(def ~name ~args (memoize (fn ~(first body) ~@(rest body))))
    `(def ~name (memoize (fn ~args ~@body)))))

;;; Lazy variables

(defmacro deflz "Like `def` but will only compute value on demand."
  [var & body]
  `(def ~var (delay ~@body)))

;;; ⩇⩆⩇ Exceptions ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn error "Throw a generic Exception with formatted string"
  [s & args]
  (throw (Exception. (apply format s args))))

(defn warn [s & args]
  (println (str "WARNING: " (apply format s args))))

(defmacro ignore-errors "Execute `body`; if an exception occurs return `nil`. Note: strongly deprecated for production code."
  [& body]
  `(try (do ~@body)
        (catch Throwable e# nil)))

(defmacro ignore-report "Execute `body`, if an exception occurs, print a message and continue"
  [& body]
  `(try (do ~@body)
        (catch Throwable e# (warn (str "Ignored error: " (.getMessage e#))))))

(defn error-handling-fn
  "Returns a fn that acts like f, but return value is (true result) or (false errmsg) in the case of an error"
  [f]
  (fn [& args]
    (try
      (let [res (apply f args)]
        (list true res))
      (catch Exception e
        (list false (str "Caught exception: " e)) ))))

(defn timing-fn
  "Returns a fn that acts like f, but return value is (time result), time in msec]"
  [f]
  (fn [& args]
    (let [start (. System (nanoTime))
          ret (apply f args)]
      (list (/ (double (- (. System (nanoTime)) start)) 1000000.0)
            ret))))

(defn java-resource->string [resource]
  (-> resource
      clojure.java.io/resource
      clojure.java.io/input-stream
      slurp))

;;; ⩇⩆⩇ Files ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn content-files
  [dir & regex]
  (filter #(and (not (.isDirectory %))
                (or (empty? regex) (re-find (first regex) (str %))))
          (file-seq (clojure.java.io/file dir))))

(defn file-exists?
  "True if file `path` exists"
  [path]
  (.exists (clojure.java.io/as-file path)))

(defn file-delete-recursively
  "Delete a directory and its contents"
  [fname]
  (letfn [(del1 [f]
            (when (.isDirectory f)
              (doseq [f2 (.listFiles f)]
                (del1 f2)))
            (clojure.java.io/delete-file f))]
    (del1 (clojure.java.io/file fname))))

(defn file-delete-safe
  "Delete a file or directory safely (that is, no error if doesn't exist)"
  [fname]
  (when (file-exists? fname)
    (file-delete-recursively fname)))

;;; http://stackoverflow.com/questions/840190/changing-the-current-working-directory-in-java
(defn cd "As in Unix shell cd"
  [dirname]
  (let [dir (.getAbsoluteFile (java.io.File. dirname))]
    (System/setProperty "user.dir" (.getAbsolutePath dir))
    dir))

(defn temp-file []
  (java.io.File/createTempFile "temp" ""))

(defn temp-file-path []
  (.getPath (temp-file)))

(defn temp-dir-path []
  (str (java.nio.file.Files/createTempDirectory "temp" (into-array java.nio.file.attribute.FileAttribute [] ))))

(defn file-lines [file]
  (let [r (clojure.java.io/reader file)]
    (line-seq r)))

(defn file-lines-out [file seq]
  (let [w (clojure.java.io/writer file)]
    (binding [*out* w]
      (doseq [l seq]
        (println l)))))

(defn process-file-lines [f in out]
  (file-lines-out out (map f (file-lines in))))

(defn directory-files [d filterfn]
  (filter #(and (not (.isDirectory %))
                (.exists %)
                (filterfn (.getName %)))
          (file-seq (clojure.java.io/file d))))

(defn ensure-directory
  "Create directory if it doesn't exist (recursively)"
  [d]
  (let [f (java.io.File. d)]
    (when-not (.exists f)
      (.mkdirs f))))

(declare clean-map)

(defn read-tsv-file
  "Given a tsv file with a header line, returns seq where each elt is a map of field names to strings"
  [f]
  (let [raw (file-lines f)
        fields (str/split (first raw) #"\t")]
    (map (fn [l]
           (clean-map
            (zipmap fields (str/split l #"\t"))
            #(= % "")))
         (rest raw))))


;;; ⩇⩆⩇ Strings ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn string> [s1 s2]
  (> (compare s1 s2) 0))

;;; +++ must be a more standard form
(defn string-search [string sub]
  (let [pos (.indexOf string sub)]
    (if (> pos 0)
      pos
      false)))

(defn string-search-all [string sub & [start]]
  (let [pos (.indexOf string sub (or start 0))]
    (if (> pos 0)
      (cons pos (string-search-all string sub (+ 1 pos)))
      ())))

(defn underscore->camelcase
  [s]
  (apply str (map str/capitalize (str/split s #"_"))))

;;; TODO camelcase->underscore

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
       (re-seq  #"[\p{L}'\d]+" s)))

(defn bigrams [tokens]
  (map list tokens (rest tokens)))

(defn file-tokens [f]
  (mapcat tokens (file-lines f)))

(defn remove-stops
  "Remove stop words from a string. Stops is a set of stop words"
  [string stops]
  (str/join
   " "
   (remove #(get stops %) (tokens string))))

;;; ⩇⩆⩇ Sequences and Maps ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Trying to introduce the convention of an = suffix meaning take a value for equality test instead of a predicate.
(defn remove= [elt seq]
  (remove #(= % elt) seq))

(defn positions "Returns a list of indexes of coll for which pred is  true (if predicate)"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x) idx))
                coll))

(defn positions= [elt coll]
  (positions #(= % elt) coll))

;;; From https://github.com/clojure/core.incubator/blob/master/src/main/clojure/clojure/core/incubator.clj
(defn seqable?
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (.isArray (.getClass ^Object x))
      (string? x)
      (instance? java.util.Map x)))

(defn nullish? [v]
  "True if value is something we probably don't care about (nil, false, empty seqs, empty strings)"
  (or (false? v) (nil? v) (and (seqable? v) (empty? v))))

(defn clean-map 
  "Remove values from 'map' based on 'pred' (default is `nullish?`)"
  ([map] (clean-map map nullish?))
  ([map pred] (select-keys map (for [[k v] map :when (not (pred v))] k))))

(defn cl-find [val sequence & {xkey :key, xtest :test, :or {xkey identity, xtest =}}]
  (apply (some-fn #(and (xtest (xkey %) val) %)) sequence))

(defn sequencify [thing]
  (if (sequential? thing)
    thing
    (list thing)))

(defn map-invert-multiple
  "Returns the inverse of map with the vals mapped to the keys. Like clojure.set/map-invert, but does the sensible thing with multiple values.
Ex: `(map-invert-multiple  {:a 1, :b 2, :c [3 4], :d 3}) ==>⇒ {2 #{:b}, 4 #{:c}, 3 #{:c :d}, 1 #{:a}}`"
  [m]
  (let [unset
        (reduce (fn [m [k v]]
            (reduce (fn [mm elt]
                      (assoc mm elt (cons k (get mm elt))))
                    m
                    (sequencify v)))
          {}
          m)]
    (zipmap (keys unset) (map #(into #{} (get unset %)) (keys unset)))))

;;; TODO versions of these that can take comparator as arg
(defn max-by "Find the maximim element of `seq` based on keyfn"
  [keyfn seq]
  (reduce (fn [a b] (if (> (keyfn a) (keyfn b)) a b))
          seq))

(defn min-by "Find the minimum element of `seq` based on keyfn"
  [keyfn seq]
  (reduce (fn [a b] (if (< (keyfn a) (keyfn b)) a b))
          seq))

(defn lunion "Compute the union of `lists`"
  [& lists]
  (apply clojure.set/union lists))      ;set fn works here, but not for other cases

(defn lintersection "Compute the intersection of `lists`"
  [& lists]
  (seq (apply clojure.set/intersection (map set lists))))

(defn lset-difference "Compute the set difference of `list1` - `list2'"
  [list1 list2]
  (seq (clojure.set/difference (set list1) (set list2))))

(defn transitive-closure [f]
  "f is a fn of one arg that returns a list. Returns a new fn that computes the transitive closure."
  (fn [root]
    (loop [done (set nil)
           fringe (list root)]
      (if (empty? fringe)
        done
        (let [expanded (first fringe)
              expansion (f expanded)
              new (clojure.set/difference (set expansion) done)]
          (recur (clojure.set/union done (set (list expanded)))
                 (concat new (rest fringe))))))))

;;; Previously called take-groups
(defn partition-lossless
  "Like partition, but include the final partial subset!"
  [n l]
  (partition n n '() l))

(defn map-chunked "Call f with chunk-sized subsequences of l, concat the results"
  [f chunk-size l]
  (mapcat f (partition-lossless chunk-size l)))

(defn sort-map-by-values [m]
  (into (sorted-map-by (fn [k1 k2] (compare [(get m k2) k2] [(get m k1) k1]))) m))

(defn freq-map [seq]
  (sort-map-by-values (reduce (fn [m v] (assoc m v (if-let [o (get m v)] (+ 1 o) 1))) {} seq)))

;;; Skips words that are not in base-freq tble
(defn overexpressed [freq base-freq]
  (sort-map-by-values
   (reduce (fn [m [k v]]
             (if-let [base (get base-freq k)]
               (assoc m k (/ v base))
               m)) {} freq)))

;;; ⩇⩆⩇ Naive handy statistics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn mean "Return the arithmetic mean of the elements of `seq`"
  [seq]
  (/ (reduce + seq)
     (count seq)))

(defn standard-deviation "Return standard deviation of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (mean (map #(Math/pow (- % mean0) 2) seq))))

(defn geometric-mean "Return the geometric mean of the elements of `setq`"
  [seq]
  (Math/pow (reduce * (map double seq))
            (/ 1 (count seq))))

(def primes
  (cons 2
        (lazy-seq
         (filter
          (fn [n] (not (some (fn [i] (= 0 (rem n i)))
                             (take-while #(<= % (Math/sqrt n)) primes))))
          (iterate #(+ % 2) 3)
          ))))

;;; ⩇⩆⩇ Date/time ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; There's a clj-time package that is no doubt betterk i

(defn-memoized date-formatter [f]
  (java.text.SimpleDateFormat. f))

; "yy-MM-dd kk:mm"
; "YYYY-MM-dd_HH_MM_SS")
(defn date-format [date format]
  (.format (date-formatter format) date))

(defn date+ [date days hours minutes]
  (java.util.Date. (+ (.getTime date) (* 60 1000 (+ minutes (* 60 (+ hours (* 24 days))))))))

;;; ⩇⩆⩇ Output ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn humanize-number [n]
  (cond (>= n 1e9)
        (format "%1.3gG" (/ n 1e9))
        (>= n 1e6)
        (format "%1.3gM" (/ n 1e6))
        (>= n 1e3)
        (format "%1.3gk" (/ n 1e3))
        true
        (str n)
        ))

;;; ⩇⩆⩇ Media ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; TODO OSX only, how do you conditionalize?
(defn speak [text]
 (let [engine (.getScriptEngine (apple.applescript.AppleScriptEngineFactory.))]
   (.eval engine (format "say \"%s\"" text))))

;;; ⩇⩆⩇ Shell ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn sh-errchecked [& args]
  (let [res (apply clojure.java.shell/sh args)]
    (when-not (= (:exit res) 0)
      (throw (Exception. "Bad result from shell" res))
      )))
