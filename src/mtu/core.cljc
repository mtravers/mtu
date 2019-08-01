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
(defmacro defn-memoized "Like `defn`, but produces a memoized function"
  [name args & body]
  ;; This crock is because you can't have multiple varadic arg signatures...sigh
  (if (string? args)
    `(def ~name ~args (memoize (fn ~(first body) ~@(rest body))))
    `(def ~name (memoize (fn ~args ~@body)))))

(defmacro def-lazy "Like `def` but will only compute value on demand."
  [var & body]
  `(def ~var (delay ~@body)))

;;; ⩇⩆⩇ Error handling ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defmacro ignore-errors "Execute `body`; if an exception occurs return `nil`. Note: strongly deprecated for production code."
  [& body]
  `(try (do ~@body)
        (catch #?(:clj Throwable :cljs :default) e# nil)))

(defmacro ignore-report "Execute `body`, if an exception occurs, print a message and continue"
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



(defn parse-numeric
  [s]
  #?(:cljs (js/parseFloat s)
     :clj (Float. s)))



(defn underscore->camelcase
  [s]
  (apply str (map str/capitalize (str/split s #"_"))))

(defn re-find-all
  "Find all matches in a string."
  [re s]
  (let [m (re-matcher re s)
        collector (atom [])]
    (while (.find m)
      (swap! collector conj (re-groups m)))
    @collector))

(defn re-pattern-literal
  [s]
  #?(:clj  
     (re-pattern (java.util.regex.Pattern/quote s))
     :cljs
     (re-pattern (str/replace s #"([)\/\,\.\,\*\+\?\|\(\)\[\]\{\}\\])" "\\$1"))))

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

;;; ⩇⩆⩇ Sequences and Maps ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

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

;;; isn't this in clj.core somewhere
(defn partition-with [pred coll]
  [(filter pred coll) (remove pred coll)])

(defn nullish? 
  "True if value is something we probably don't care about (nil, false, empty seqs, empty strings)"
  [v]
  (or (false? v) (nil? v) (and (seqable? v) (empty? v))))

(defn clean-map 
  "Remove values from 'map' based on 'pred' (default is `nullish?`)"
  ([map] (clean-map map nullish?))
  ([map pred] (select-keys map (for [[k v] map :when (not (pred v))] k))))

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
                                                   existing? #{}
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

;;; TODO? could work like regular merge and prefer m2 when unmergeable
(defn merge-recursive [m1 m2]
  (cond (and (map? m1) (map? m2))
        (merge-with merge-recursive m1 m2)
        (nil? m1) m2
        (nil? m2) m1
        (= m1 m2) m1
        ;; TODO? might want a version that combined these into a vector or something similar
        true (throw (Exception. (str "Can't merge " m1 " and " m2)))))

(defn map-values [f hashmap]
  (zipmap (keys hashmap) (map f (vals hashmap))))

;;; Alternate implementation
#_
(defn map-values [f hashmap]
  (reduce-kv (fn [m k v]
               (assoc m k (f v)))
             {} hashmap))


(defn map-keys [f hashmap]
  (zipmap (map f (keys hashmap)) (vals hashmap)))

(defn index-by 
  [f coll]  
  (zipmap (map f coll) coll))

(defn dissoc-if [f hashmap]
  (apply dissoc hashmap (map first (filter f hashmap))))

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

(defn map-diff [a b]
  (let [both (set/intersection (set (keys a)) (set (keys b)))
        a-only (set/difference both (set (keys a)))
        b-only (set/difference both (set (keys b)))]
    (prn [:a-only a-only])
    (prn [:a-only b-only])
    (for [k both]
      (when-not (= (k a) (k b))
        (prn [:slot-diff k (k a) (k b)])))))

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

(defn score-by "Return a list of [elt score] pairs, in descending score order."
  [keyfn seq]
  (reverse
   (sort-by second
            (map (fn [elt] [elt (keyfn elt)]) seq))))

(defn outliers-by "Return elements of `seq` on whom `scorefn` is more than `factor` standard-deviations away from the mean."
  [scorefn seq factor]
  (let [scores (map scorefn seq)
        threshold (+ (mean scores) (* factor (standard-deviation scores)))]
    (filter identity (map (fn [elt score] (when (>= score threshold) elt)) seq scores))))

;;; A highly useful and underused statistic
(defn coefficent-of-variation "Return coefficent of variation of the elements of `seq`"
  [seq]
  (/ (standard-deviation seq)
     (mean seq)))

(defn safe-round [n]
  (if (int? n) n (Math/round n)))

(defn iles [seq n]
  (let [sorted (into [] (sort seq))
        count (double (count seq))]
    (map #(nth sorted (Math/round (* % (/ count n))))
         (range 1 n))))
    
(defn geometric-mean "Return the geometric mean of the elements of `seq`"
  [seq]
  (Math/pow (reduce * (map double seq))
            (/ 1 (count seq))))

(defn outliers-by
  [scorefn seq factor]
  (let [scores (map scorefn seq)
        threshold (+ (mean scores) (* factor (standard-deviation scores)))]
    (filter identity (map (fn [elt score] (when (>= score threshold) elt)) seq scores))))

(def primes
  (cons 2
        (lazy-seq
         (filter
          (fn [n] (not (some (fn [i] (= 0 (rem n i)))
                             (take-while #(<= % (Math/sqrt n)) primes))))
          (iterate #(+ % 2) 3)
          ))))


;;; I need this as an omnipresent dev tool (TODO think there is something similar built into Clojure 1.10, tap)
(def captures (atom {}))

(defn capture [tag thing]
  (swap! captures assoc tag thing)
  thing)

;;;
(defn mapped [f] (fn [& args] (apply map f args)))
(def +* (mapped +))
;; etc

(defn spittoon
  "Like spit, but will print lazy lists usefully and completely"
  [f thing]
  (binding [*print-length* nil]
    (spit f (pr-str thing))))
        
;;; For Amuedo – already exists as range, but that cheats and uses Java
(defn ramp [beg end inc]
  (take-while (partial > end)
              (iterate (partial + inc) beg)))

;;; Slightly less cryptically
(defn ramp [beg end inc]
  (take-while (fn [x] (> end x))
              (iterate (fn [x] (+ inc x)) beg)))





