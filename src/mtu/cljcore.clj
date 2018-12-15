(ns mtu.cljcore
  "Java-only stuff"
  (:require
   clojure.java.shell
   [clojure.string :as str]
   [mtu.core :as core]
  ))

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

(defn local-file [url]
  (let [url (java.net.URL. url)
        tmp (temp-file)]
    (clojure.java.io/copy (.openStream url) tmp)
    (.getPath tmp)))


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

;;; TODO generalize to more than one form?
(defn read-from-file
  "Read a form from a file"
  [file]
  (let [rdr (-> file
                clojure.java.io/file
                clojure.java.io/reader
                java.io.PushbackReader.)]
    (read rdr)))

;;; ⩇⩆⩇ Date/time ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; There's a clj-time package that is no doubt better

(core/defn-memoized date-formatter [f]
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

;;; ⩇⩆⩇ Shell ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn sh-errchecked [& args]
  (let [res (apply clojure.java.shell/sh args)]
    (when-not (= (:exit res) 0)
      (throw (Exception. "Bad result from shell" res))
      )))

;;; ⩇⩆⩇ Higher file fns ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn read-tsv-file
  "Given a tsv file with a header line, returns seq where each elt is a map of field names to strings"
  [f]
  (let [raw (file-lines f)
        fields (str/split (first raw) #"\t")]
    (map (fn [l]
           (core/clean-map
            (zipmap fields (str/split l #"\t"))
            #(= % "")))
         (rest raw))))

(defn file-tokens [f]
  (mapcat core/tokens (file-lines f)))

(defn open-url [url]
  (.browse (java.awt.Desktop/getDesktop)
           (java.net.URI/create url)))
