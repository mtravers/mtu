(ns mtu.dev
  "Utility functions for development only"
  (:require clojure.pprint)
  )

;;; ⩇⩆⩇ Debugging/interaction ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defmacro dbg "Wrap this around an expression; will print expression and value when run."
  [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;; TODO there is something similar built into Clojure 1.10, tap>, maybe use that.

(defn tapr "Print `thing` and  return it as value"
  [thing]
  (prn thing)
  thing)

(def captures (atom {}))

(defn capture [tag thing]
  (swap! captures assoc tag thing)
  thing)

;;; TODO – implement equiv of CL mt/plet

(defn pp [thing]
  (clojure.pprint/pprint thing))

(defn get-methods "Return a list of all method names on `class`"
  [class]
  (distinct (map #(.getName %) (seq (.getMethods class)))))

;; The built in clojure.repl/apropos manages to not return the namespace; this version fixes that.
;; Seems to have been fixed in mainstream clojure? So this may be obsolete.
#?(:clj
   (defn apropos
     "Given a regular expression or stringable thing, return a seq of
  all definitions in all currently-loaded namespaces that match the
  str-or-pattern."
     [str-or-pattern]
     (let [matches? (if (instance? java.util.regex.Pattern str-or-pattern)
                      #(re-find str-or-pattern (str %))
                      #(.contains (str %) (str str-or-pattern)))]
       (mapcat (fn [ns]
                 (map #(symbol (str ns) (str %)) (filter matches? (keys (ns-publics ns)))))
               (all-ns)))))

(defn class-source "Return the jar file that defines a given class"
  [klass]
  (.getLocation (.getCodeSource (.getProtectionDomain klass))))

