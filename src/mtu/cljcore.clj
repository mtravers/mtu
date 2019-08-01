(ns mtu.cljcore
  "Java-only stuff"
  (:require
   clojure.java.shell
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [mtu.core :as core]
  ))

;;; ⩇⩆⩇ Exceptions ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn error "Throw a generic Exception with formatted string"
  [s & args]
  (throw (Exception. (apply format s args))))

(defn warn [s & args]
  (println (str "WARNING: " (apply format s args))))

;;; TODO port to .cljc using window.performance.now() (returns usec)
(defn timing-fn
  "Returns a fn that acts like f, but return value is (time result), time in msec]"
  [f]
  (fn [& args]
    (let [start (System/nanoTime)
          ret (apply f args)]
      (list (/ (double (- (System/nanoTime) start)) 1000000.0)
            ret))))

(defn java-resource->string [resource]
  (-> resource
      io/resource
      io/input-stream
      slurp))

;;; ⩇⩆⩇ Files ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Note: probably better to avoid these and use raynes/fs https://github.com/Raynes/fs

(defn content-files
  [dir & regex]
  (filter #(and (not (.isDirectory %))
                (or (empty? regex) (re-find (first regex) (str %))))
          (file-seq (io/file dir))))

(defn file-exists?
  "True if file `path` exists"
  [path]
  (.exists (io/as-file path)))

(defn file-delete-recursively
  "Delete a directory and its contents"
  [fname]
  (letfn [(del1 [f]
            (when (.isDirectory f)
              (doseq [f2 (.listFiles f)]
                (del1 f2)))
            (io/delete-file f))]
    (del1 (io/file fname))))

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

(defn directory-files [d filterfn]
  (filter #(and (not (.isDirectory %))
                (.exists %)
                (filterfn (.getName %)))
          (file-seq (io/file d))))

(defn ensure-directory
  "Create directory if it doesn't exist (recursively)"
  [d]
  (let [f (java.io.File. d)]
    (when-not (.exists f)
      (.mkdirs f))))


(defn local-file [url]
  (let [url (java.net.URL. url)
        tmp (temp-file)]
    (io/copy (.openStream url) tmp)
    (.getPath tmp)))

(defn file-lines [file]
  (let [r (io/reader file)]
    (line-seq r)))

(defn file-lines-out [file seq]
  (let [w (io/writer file)]
    (binding [*out* w]
      (doseq [l seq]
        (println l)))))

(defn process-file-lines [f in out]
  (file-lines-out out (map f (file-lines in))))

;;; TODO generalize to more than one form?
(defn read-from-file
  "Read a form from a file"
  [file]
  (let [rdr (-> file
                io/file
                io/reader
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
  (let [res (apply shell/sh args)]
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

(defn open-url
  [url]
  (when (java.awt.Desktop/isDesktopSupported)
    (.browse (java.awt.Desktop/getDesktop)
             (java.net.URI/create url))))

;;; +++ must be a more standard form
;;; TODO if actually useful, port to cljc
(defn string-search
  [string sub]
  (let [pos (.indexOf string sub)]
    (if (> pos 0)
      pos
      false)))

(defn string-search-all
  [string sub & [start]]
  (let [pos (.indexOf string sub (or start 0))]
    (if (> pos 0)
      (cons pos (string-search-all string sub (+ 1 pos)))
      ())))

;;; Conceivably do dates as well
(defn coerce-value
  "Attempt to turn a string into a number (int or float). Return number if succesful, otherwise original string"
  [str]
  (if-let [inum (re-matches #"-?\d+" str)]
    (try
      (Integer. inum)
      (catch Exception _
        str))
    (if-let [fnum (re-matches #"-?\d+\.?\d*" str)]
      (try
        (Float/parseFloat fnum)
        (catch Exception _
          str))
      str)))

(defn keyword-safe
  "Turn string into keyword that, replacing chars that will make serialization/deserialization problematic"
  [str]
  (keyword (str/replace str #"[ ,\(\):]" "_")))

(defn random-uuid
  []
  (str (java.util.UUID/randomUUID)))

;;; Turn a string into a regex, taking every character literally (not as regex operators)
(defn re-pattern-literal
  {:tag java.util.regex.Pattern
   :added "1.0"
   :static true}
  [s]
  (re-pattern (java.util.regex.Pattern/quote s)))

;;; Probably want keyword maps, or at least an option for that.
(defn expand-template-string
  "Template is a string containing {foo} elements, which get replaced by corresponding values from bindings"
  [template bindings]
  (let [matches (->> (re-seq #"\{(.*?)\}" template) ;extract the template fields from the entity
                     (map (fn [[match key]]
                            [match (or (bindings key) "")])))]
    (reduce (fn [s [match key]]
              (str/replace s (re-pattern-literal match) (str key)))
            template matches)))

;;; dependent on [org.clojure/data.xml "0.0.8"], just as soon not
#_
(defn parse-xml-string [s]
  (with-in-str s
    (clojure.xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn now []
  (java.util.Date.))

(defn schpit 
  "Like core/spit, but will do something sensible for lazy seqs "
  [f content & options]
  (with-open [w (apply io/writer f options)]
    (binding [*print-length* nil]
      (prn content w))))

(defn read-chars
  [reader n]
  (let [a (char-array n)]
    (.read reader a)
    (String. a)))
