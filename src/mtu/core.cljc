(ns mtu.core
  "Various generally useful utilities to keep mt sane"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   ))

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

;;; ⩇⩆⩇ Strings ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

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
       (re-seq #"[\p{L}'\d]+" s)))

(defn bigrams [tokens]
  (map list tokens (rest tokens)))

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
#?(:clj
   (defn seqable?
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (.isArray (.getClass ^Object x))
      (string? x)
      (instance? java.util.Map x))))

(defn nullish? [v]
  "True if value is something we probably don't care about (nil, false, empty seqs, empty strings)"
  (or (false? v) (nil? v) (and (seqable? v) (empty? v))))

(defn clean-map 
  "Remove values from 'map' based on 'pred' (default is `nullish?`)"
  ([map] (clean-map map nullish?))
  ([map pred] (select-keys map (for [[k v] map :when (not (pred v))] k))))

(defn cl-find [val sequence & {xkey :key, xtest :test, :or {xkey identity, xtest =}}]
  (apply (some-fn #(and (xtest (xkey %) val) %)) sequence))

(defn distinctly
  "Like distinct, but equality determined by keyfn"
  [coll keyfn]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen (keyfn f))
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen (keyfn f)))))))
                 xs seen)))]
    (step coll #{})))

(defn sequencify [thing]
  (if (sequential? thing)
    thing
    (list thing)))

(defn filter-rest
  "A lazy sequence generated by applying f to seq and its tails"
  [f seq]
  (lazy-seq
   (cond
     (empty? seq) []
     (f seq) (cons seq (filter-rest f (rest seq)))
     true (filter-rest f (rest seq)))))

(defn map-values [f hashmap]
  (zipmap (keys hashmap) (map f (vals hashmap))))

(defn map-keys [f hashmap]
  (zipmap (map f (keys hashmap)) (vals hashmap)))

(defn map-invert-multiple
  "Returns the inverse of map with the vals mapped to the keys. Like set/map-invert, but does the sensible thing with multiple values.
Ex: `(map-invert-multiple  {:a 1, :b 2, :c [3 4], :d 3}) ==>⇒ {2 #{:b}, 4 #{:c}, 3 #{:c :d}, 1 #{:a}}`"
  [m]
  (map-values
   (partial into #{})
   (reduce (fn [m [k v]]
            (reduce (fn [mm elt]
                      (assoc mm elt (cons k (get mm elt))))
                    m
                    (sequencify v)))
          {}
          m)))

(defn map-diff [a b]
  (let [both (set/intersection (set (keys a)) (set (keys b)))
        a-only (set/difference both (set (keys a)))
        b-only (set/difference both (set (keys b)))]
    (prn [:a-only a-only])
    (prn [:a-only b-only])
    (for [key both]
      (when-not (= (key a) (key b))
        (prn [:slot-diff key (key a) (key b)])))))

(defn >*
  "Generalization of > to work on anything with a compare fn"
  ([a b]
   (> (compare a b) 0))
  ([a b & rest]
   (and (>* a b)
        (apply >* b rest))))

(defn <*
  "Generalization of < to work on anything with a compare fn"
  ([a b]
   (< (compare a b) 0))
  ([a b & rest]
   (and (<* a b)
        (apply <* b rest))))

(defn max-by "Find the maximim element of `seq` based on keyfn"
  [keyfn seq]
  (reduce (fn [a b] (if (>* (keyfn a) (keyfn b)) a b))
          seq))

(defn min-by "Find the minimum element of `seq` based on keyfn"
  [keyfn seq]
  (reduce (fn [a b] (if (<* (keyfn a) (keyfn b)) a b))
          seq))

(defn lunion "Compute the union of `lists`"
  [& lists]
  (apply set/union lists))      ;set fn works here, but not for other cases

(defn lintersection "Compute the intersection of `lists`"
  [& lists]
  (seq (apply set/intersection (map set lists))))

(defn lset-difference "Compute the set difference of `list1` - `list2'"
  [list1 list2]
  (seq (set/difference (set list1) (set list2))))

(defn transitive-closure 
  "f is a fn of one arg that returns a list. Returns a new fn that computes the transitive closure."
  [f]
  (fn [root]
    (loop [done (set nil)
           fringe (list root)]
      (if (empty? fringe)
        done
        (let [expanded (first fringe)
              expansion (f expanded)
              new (set/difference (set expansion) done)]
          (recur (set/union done (set (list expanded)))
                 (concat new (rest fringe))))))))

(defn powerset
  "Compute powerset of a set"
  [s]
  (if (empty? s) #{#{}}
      (let [elt (set (list (first s)))
            tail (powerset (rest s))]
        (set/union (into #{} (map #(set/union elt %) tail))
                   tail))))

;;; partition-lossless
;;; Previously called take-groups
;;; and turns out to be subsumed by clojure.core/partition-all

(defn map-chunked "Call f with chunk-sized subsequences of l, concat the results"
  [f chunk-size l]
  (mapcat f (partition-all chunk-size l)))

(defmacro doseq* "Like doseq, but goes down lists in parallel rather than nested. Assumes lists are same size."
  [bindings & body]
  (let [bindings (partition 2 bindings)
        vars (map first bindings)
        forms (map second bindings)
        lvars (map gensym vars)]
    `(loop ~(into [] (mapcat (fn [v f] [v f]) lvars forms))
       (let ~(into [] (mapcat (fn [v lv] [v `(first ~lv)]) vars lvars))
         ~@body
         (when-not (empty? (rest ~(first lvars)))
           (recur ~@(map (fn [lv] `(rest ~lv)) lvars)))
         ))))

(defmacro for*
  "Like for but goes down lists in parallel rather than nested. Assumes lists are same size."
  [bindings & body]
  (let [bindings (partition 2 bindings)
        vars (map first bindings)
        forms (map second bindings)
        lvars (map gensym vars)]
    `(map (fn ~(into [] vars) ~@body) ~@forms)))

(defn sort-map-by-values [m]
  (into (sorted-map-by (fn [k1 k2] (compare [(get m k2) k2] [(get m k1) k1]))) m))

(defn freq-map [seq]
  (sort-map-by-values (frequencies seq)))

(defn clump-by
  "Sequence is ordered (by vfn), comp is a comparator between two elts. Returns groups in which comp is true for consecutive elements"
  [sequence vfn comp]
  (if (empty? sequence)
    sequence
    (reverse
     (map reverse
          (reduce (fn [res b]
                    (let [a (first (first res))]
                      (if (comp (vfn a) (vfn b))
                        (cons (cons b (first res)) (rest res))
                        (cons (list b) res))))
                  (list (list (first sequence)))
                  (rest sequence))))))

;;; ⩇⩆⩇ Naive handy statistics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn mean "Return the arithmetic mean of the elements of `seq`"
  [seq]
  (/ (reduce + seq)
     (count seq)))

(defn standard-deviation "Return standard deviation of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (Math/sqrt
     (/ (reduce + (map #(Math/pow (- % mean0) 2) seq))
        (- (count seq) 1)))))

;;; A highly useful and underused statistic
(defn coefficent-of-variation "Return coefficent of variation of the elements of `seq`"
  [seq]
  (/ (standard-deviation seq)
     (mean seq)))

(defn geometric-mean "Return the geometric mean of the elements of `seq`"
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


