(ns mtu.core
  "Various generally useful utilities to keep mt sane"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.walk :as walk]
   ))

;;; (many based on CL; see https://github.com/mtravers/mtlisp/blob/master/mt-utils.lisp )

;;; ⩇⩆⩇ Memoization ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See https://github.com/clojure/core.memoize/ for more memoize hacks
(defmacro defn-memoized
  "Like `defn`, but produces a memoized function"
  [name args & body]
  ;; This crock is because you can't have multiple varadic arg signatures...sigh
  (if (string? args)
    `(def ~name ~args (memoize (fn ~(first body) ~@(rest body))))
    `(def ~name (memoize (fn ~args ~@body)))))

(defmacro def-lazy
  "Like `def` but will delay computing value until it is demanded."
  [var & body]
  `(def ~var (delay ~@body)))

;;; ⩇⩆⩇ Error handling ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defmacro ignore-errors
  "Execute `body`; if an exception occurs return `nil`. Note: strongly deprecated for production code."
  [& body]
  `(try (do ~@body)
        (catch #?(:clj Throwable :cljs :default) e# nil)))

(defmacro ignore-report
  "Execute `body`, if an exception occurs, print a message and continue"
  [& body]
  `(try (do ~@body)
        (catch #?(:clj Throwable :cljs :default) e# (warn (str "Ignored error: " (.getMessage e#))))))

(defn error-handling-fn
  "Returns a fn that acts like f, but return value is (true result) or (false errmsg) in the case of an error"
  [f]
  (fn [& args]
    (try
      (let [res (apply f args)]
        (list true res))
      (catch  #?(:clj Throwable :cljs :default) e
        (list false (str "Caught exception: " e))))))

;;; ⩇⩆⩇ Strings ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇


;;; Replaced with more powerful coerce-numeric
#_
(defn parse-numeric
  [s]
  #?(:cljs (js/parseFloat s)
     :clj (Float. s)))

(defn coerce-numeric
  "Attempt to turn str into a number (long or double).
  Return number if succesful, otherwise original string"
  [str]
  (when str
    (if-let [inum (re-matches #"-?\d+" str)]
      (try
        #?(:cljs (js/parseInt inum)
           :clj (Long. inum))
        (catch #?(:clj Throwable :cljs :default)  _ str))
      (if-let [fnum (re-matches #"-?\d*\.?\d*" str)]
        (try
          #?(:cljs (js/parseFloat fnum)
             :clj (Double. fnum))
          (catch #?(:clj Throwable :cljs :default) _ str))
        str))))

(defn underscore->camelcase
  [s]
  (apply str (map str/capitalize (str/split s #"_"))))

;;; ⩇⩆⩇ Regex and templating ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn re-find-all
  "Find all matches in a string."
  [re s]
  #?(:clj
     (let [m (re-matcher re s)
           collector (atom [])]            ;TODO better to use transients
       (while (.find m)
         (swap! collector conj (re-groups m)))
       @collector)
     :cljs
     (throw (ex-info "TODO" {}))))

(defn re-quote
  [s]
  #?(:clj  
     (java.util.regex.Pattern/quote s)
     :cljs
     (str/replace s #"([)\/\,\.\,\*\+\?\|\(\)\[\]\{\}\\])" "\\$1")))

(defn re-pattern-literal [string]
  (re-pattern (re-quote string)))

(defn re-pattern-literal-token [string]
  (re-pattern (str "\\Wstring\\W")))

(defn expand-template-string
  "Template is a string containing {foo} elements, which get replaced by corresponding values from bindings"
  [template bindings]
  (let [matches (->> (re-seq #"\{(.*?)\}" template) ;extract the template fields from the entity
                     (map (fn [[match key]]
                            [match (or (bindings key) "")])))]
    (reduce (fn [s [match key]]
              (str/replace s (re-pattern-literal match) (str key)))
            template matches)))

;;; TODO camelcase->underscore

;;; ⩇⩆⩇ Keywords and namespaces ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn keyword-safe
  "Make a string into a readable keyword by replacing certain punctuation"
  [str]
  (keyword (str/replace str #"[ ,\(\):]" "_")))

(defn dens
  "Remove the namespaces that backquote insists on adding"
  [struct]
  (walk/postwalk 
   #(if (symbol? %)
    (symbol nil (name %))
    %)
   struct))


;;; ⩇⩆⩇ Variations on standard predicates ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn nullish? 
  "True if value is something we probably don't care about (nil, false, empty seqs, empty strings)"
  [v]
  (or (false? v) (nil? v) (and (seqable? v) (empty? v))))

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

;;; ⩇⩆⩇ Sequences ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn doall-safe
  "Realize lazy sequences, if arg is such, otherwise a no-op."
  [thing]
  (if (sequential? thing)
    (doall thing)
    thing))

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

;;; TODO Isn't this already in clj.core somewhere?
(defn partition-with [pred coll]
  ((juxt filter remove) pred seq))

;;; Stolen from https://gitlab.com/kenrestivo/utilza
(defn map-filter
  "Applies f to coll. Returns a lazy sequence of the items in coll for which
   all the results that are truthy. f must be free of side-effects."
  [f coll]
  (remove nullish? (map f coll)))

;;; Formerly deselect
(defn clean-map
  "Remove values from 'map' based on 'pred' (default is `nullish?`). "
  ([map] (clean-map map nullish?))
  ([map pred] (select-keys map (for [[k v] map :when (not (pred v))] k))))

(defn clean-walk
  "Remove values from all maps in 'struct' based on 'pred' (default is `nullish?`). "
  ([struct] (clean-walk struct nullish?))
  ([struct pred] 
   (walk/postwalk #(if (map? %) (clean-map % pred) %) struct)))


(defn cl-find
  [val sequence & {xkey :key, xtest :test, :or {xkey identity, xtest =}}]
  (apply (some-fn #(and (xtest (xkey %) val) %)) sequence))

(defn iterate-until [f start pred]
  (if (pred start)
    start
    (iterate-until f (f start) pred)))

(defn position
  [elt coll]
  (first
   (keep-indexed (fn [idx x]
                   (when (= x elt) idx))
                 coll)))

(defn remove-elt [coll elt]
  (remove #(= % elt) coll))

(defn safe-nth
  [col n]
  (and (<= 0 n (count col))
       (nth col n)))

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

(defn uncollide
  "new-key-fn is from elts to elts"
  [seq & {:keys [key-fn new-key-fn existing] :or {key-fn identity
                                                  existing #{}
                                                  }}]
  (letfn [(step [xs seen]
            (cond (empty? xs) xs
                  (contains? seen (key-fn (first xs)))
                  (let [new-elt (iterate-until new-key-fn (first xs) #(not (contains? seen (key-fn %))))]
                    (cons new-elt
                          (step (rest xs) (conj seen (key-fn new-elt)))))
                  true
                  (cons (first xs)
                        (step (rest xs) (conj seen (key-fn (first xs)))))))]
    (step seq (set existing))))

(defn sequencify [thing]
  (if (sequential? thing)
    thing
    (list thing)))

(defn unlist [thing]
  (if (and (sequential? thing) (= 1 (count thing)))
    (first thing)
    thing))

(defn filter-rest
  "A lazy sequence generated by applying f to seq and its tails"
  [f seq]
  (lazy-seq
   (cond
     (empty? seq) []
     (f seq) (cons seq (filter-rest f (rest seq)))
     true (filter-rest f (rest seq)))))

;;; TODO use transients as in group-by
(defn group-by-multiple
  "Like group-by, but f produces a seq of values rather than a single one"
  [f coll]  
  (reduce
   (fn [ret x]
     (reduce (fn [ret y]
               (assoc ret y (conj (get ret y []) x)))
             ret (f x)))
   {} coll))

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



;;; partition-lossless
;;; Previously called take-groups
;;; and turns out to be subsumed by clojure.core/partition-all

(defn map-chunked "Call f with chunk-sized subsequences of l, concat the results"
  [f chunk-size l]
  (mapcat f (partition-all chunk-size l)))

(defn clump-by
  "Sequence is ordered (by vfn), comparator is a fn of two elts. Returns groups in which comp is true for consecutive elements"
  [sequence vfn comparator]
  (if (empty? sequence)
    sequence
    (reverse
     (map reverse
          (reduce (fn [res b]
                    (let [a (first (first res))]
                      (if (comparator (vfn a) (vfn b))
                        (cons (cons b (first res)) (rest res))
                        (cons (list b) res))))
                  (list (list (first sequence)))
                  (rest sequence))))))

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

;;; ⩇⩆⩇ Maps ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; TODO? could work like regular merge and prefer m2 when unmergeable
(defn merge-recursive [m1 m2]
  (cond (and (map? m1) (map? m2))
        (merge-with merge-recursive m1 m2)
        (nil? m1) m2
        (nil? m2) m1
        (= m1 m2) m1
        ;; TODO? might want a version that combined these into a vector or something similar
        true (throw (ex-info (str "Can't merge " m1 " and " m2) {}))))

(defn map-keys [f hashmap]
  (reduce-kv (fn [acc k v] (assoc acc (f k) v)) {} hashmap))

(defn map-values [f hashmap]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) {} hashmap))

;;; See utilza.core/mapify
(defn index-by 
  [f coll]  
  (zipmap (map f coll) coll))

(defn dissoc-if [f hashmap]
  (apply dissoc hashmap (map first (filter f hashmap))))

;;; Weirdly not in clojure.core
(defn dissoc-in
  [map [k & k-rest]]
  (if k-rest
    (update map k dissoc-in k-rest)
    (dissoc map k)))

(defn remove-nil-values [hashmap]
  (dissoc-if (fn [[_ v]] (not v)) hashmap))

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

(defn map-diff
  "Returns a recursive diff of two maps, which you will want to prettyprint."
  [a b]
  (let [both (set/intersection (set (keys a)) (set (keys b)))
        a-only (set/difference (set (keys a)) both)
        b-only (set/difference (set (keys b)) both)
        slot-diffs
        (for [k both
              :when (not (= (k a) (k b)))]
          (if (and (map? (k a)) (map? (k b)))
            [:slot-diff k (map-diff (k a) (k b))]
            [:slot-diff k (k a) (k b)]))]
    [:a-only a-only :b-only b-only :slot-diffs slot-diffs]))

(defn sort-map-by-values [m]
  (into (sorted-map-by (fn [k1 k2] (compare [(get m k2) k2] [(get m k1) k1]))) m))

(defn freq-map [seq]
  (sort-map-by-values (frequencies seq)))

;;; ⩇⩆⩇ Walker ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn subst
  "Walk `struct`, replacing any keys in map with corresponding value."
  [struct map]
  (walk/postwalk #(if (contains? map %) (map %) %) struct))

(defn subst-gen
  "Like `subst`, but for entries not in map, call `generator` on first occurance to generate a value"
  [struct map generator]
  (let [cache (atom map)
        generate (fn [k]
                   (let [v (generator k)]
                     (swap! cache assoc k v)
                     v))]
    (walk/postwalk #(if (contains? @cache %)
                      (@cache %)
                      (generate %))
                   struct)))

;;; TODO avoid atom with a loop?
(defn walk-collect
  "Walk f over thing and return a list of the non-nil returned values"
  [f thing]
  (let [collector (atom [])]
    (clojure.walk/postwalk
     #(do
        (when-let [v (f %)]
          (swap! collector conj v))
        %)
     thing)
    @collector))

;;; ⩇⩆⩇ Sets ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn powerset
  "Compute powerset of a set"
  [s]
  (if (empty? s) #{#{}}
      (let [elt (set (list (first s)))
            tail (powerset (rest s))]
        (set/union (into #{} (map #(set/union elt %) tail))
                   tail))))

;;; ⩇⩆⩇ Functional ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

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

;;; Obsoleted by vectorize, more or less
(defn *ify
  "f is a 1-arg fn on a scalar, returns a fn that will apply f either a scalar or a sequence"
  [f]
  (fn [arg]
    (if (seq? arg)
      (map f arg)
      (f arg))))

(defn *ify!
  "f is a 1-arg fn on a scalar, returns a fn that will apply f either a scalar or a sequence (non-lazily)"
  [f]
  (fn [arg]
    (if (seq? arg)
      (doall (map f arg))
      (f arg))))

;;; Vectorized fns (after SciCL)

;;; Given a fn f with scalar args, (vectorized f) is a fn that takes either scalars or vectors for any argument,
;;; doing the appropriate vectorization.
;;; All vector args should be the same length.
(defn vectorize
  [f]
  (fn [& args]
    (if-let [l (some #(and (sequential? %) (count %)) args)]
      (let [vargs (map #(if (sequential? %) (vec %) %) args)] ;vectorize args
        (mapv (fn [i]
               (apply f (map (fn [arg] (if (vector? arg) (get arg i) arg)) vargs)))
             (range l)))
      (apply f args))))

;;; Eg this yields [20 400 6000]
#_ ((vectorize *) [1 2 3] 2 [10 100 1000])

;;; ⩇⩆⩇ Randomness, basic numerics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; (things that are more for stats or geometry moved to mtu.math)

(defn rand-range [a b]
  (+ a (* (rand) (- b a))))

(defn rand-around [p range]
  (rand-range (- p range) (+ p range)))

(defn safe-round [n]
  (if (int? n) n (Math/round n)))

;;; ⩇⩆⩇ Keyword gensym ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(def key-counter (atom {}))

(defn unique-key
  "Produce a unique keyword based on root."
  [root]
  (swap! key-counter update root #(inc (or % 0)))
  (keyword (namespace root)
           (str (name root) (get @key-counter root))))


