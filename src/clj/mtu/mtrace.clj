(ns mtu.mtrace)

;;; Hierarchical tracer (after Common Lisp ctrace, link TODO)
;;; Incorporating code from clojure.tools.trace (if it was better designed, could
;;; just use it as a library, grump)


(defn ^{:private true} tracer
  "This function is called by trace. Prints to standard output, but
may be rebound to do anything you like. 'name' is optional."
  [name value]
  (println (str "TRACE" (when name (str " " name)) ": " value)))


(defn ^{:skip-wiki true} trace-fn-call
  "Traces a single call to a function f with args. 'name' is the
symbol name of the function."
  [name f args]
  (let [id (gensym "t")]
    (tracer id (str (trace-indent) (pr-str (cons name args))))
    (let [value (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
      (tracer id (str (trace-indent) "=> " (pr-str value)))
      value)))

(defn ^{:skip-wiki true} trace-var*
  "If the specified Var holds an IFn and is not marked as a macro, its
  contents is replaced with a version wrapped in a tracing call;
  otherwise nothing happens. Can be undone with untrace-var.

  In the unary case, v should be a Var object or a symbol to be
  resolved in the current namespace.

  In the binary case, ns should be a namespace object or a symbol
  naming a namespace and s a symbol to be resolved in that namespace."
  ([ns s]
     (trace-var* (ns-resolve ns s)))
  ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)]
       (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::traced not))
         (let [f @v
               vname (symbol (str ns "/" s))]
           (doto v
             (alter-var-root #(fn tracing-wrapper [& args]
                                (trace-fn-call vname % args)))
             (alter-meta! assoc ::traced f)))))))




(defn mtracer [name value]
  (prn :mtrace name value))






(defn with-tracing [f]
  (binding [clojure.tools.trace/tracer mtracer]
    (f)))

