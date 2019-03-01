(ns mtu.test.wordstats
  (:use clojure.test)
  (:use mtu.wordstats)
  (:require [clojure.string :as str]))


(deftest token-test                     ; in more ways than one!
  (is (= ["foo" "bar"] (tokens "foo  bar")))
  )
