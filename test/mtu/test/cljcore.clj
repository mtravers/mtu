(ns mtu.test.cljcore
  (:use clojure.test)
  (:use mtu.cljcore))

(deftest ignore-errors-normal-test
  (is (= 7 (ignore-errors (+ 3 4)))))

(deftest ignore-errors-rror-test
  (is (= nil (ignore-errors (/ 0 0) (+ 3 4)))))

(deftest error-handling-fn-test
  (let [ed (error-handling-fn /)]
    (is (= '(true 2/3) (ed 2 3)))
    (is (= '(false "Caught exception: java.lang.ArithmeticException: Divide by zero") (ed 2 0)))))

