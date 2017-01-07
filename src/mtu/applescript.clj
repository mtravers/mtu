(ns mtu.applescript)

;;; Interface to AppleScript (OS X only, of course)

;;; Thought maybe I could make my own version of RescueTime, but AppleScript is a horrible dog and there seems to be no standard API across apps.

(defn exec [script]
  (let [engine (.getScriptEngine (apple.applescript.AppleScriptEngineFactory.))]
    (.eval engine script)))

(defn speak [text]
  (exec (format "say \"%s\"" text)))

(defn exec-app [app script]
  (exec (format "tell application \"%s\"\n %s\n end tell" app script)))

;;; TODO not sure if this is smart enough to ignore itself
(defn active-app []
  (exec-app "System Events" "return name of first application process whose frontmost is true"))

(defmulti details (fn [app] app))

(defmethod details :default [app]
  nil)

(defmethod details "Google Chrome" [app]
  {:url (exec-app "Google Chrome" "get URL of active tab of first window")
   :title (exec-app "Google Chrome" "get title of active tab of first window")})

(defmethod details "Safari" [app]
  {:url (exec-app "Safari" "return url of front document")
   :title (exec-app "Safari" "return name of front document")})

;;; Eg
(defn monitor []
  (while true
    (let [app (active-app)]
      (prn app (details app)))
    (Thread/sleep 1000)))

;;; Scratch stuff, not working

;;; Nope, apparently firefox doesn't support applescript...loser
(defn firefox-current []
  [(exec-app "Firefox" "get URL of active tab of first window")
   (exec-app "Firefox" "get title of active tab of first window")])

;;; http://stackoverflow.com/questions/263741/using-applescript-to-grab-the-url-from-the-frontmost-window-in-web-browsers-the
;;; http://www.alfredforum.com/topic/2013-how-to-get-frontmost-tab%E2%80%99s-url-and-title-of-various-browsers/

;;; WOW this doesn't work because active-app results are not suitable for passing to tell! 
(defn generic-current []
  ;; and doesn't work for Emacs, gets error
  (exec-app (active-app) "return name of front document"))

;;; itunes – easy to get current song, but its usually not the front app
