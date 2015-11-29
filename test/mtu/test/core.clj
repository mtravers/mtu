(ns mtu.test.core
  (:use clojure.test)
  (:use mtu.core))

(deftest ignore-errors-normal-test
  (is (= 7 (ignore-errors (+ 3 4)))))

(deftest ignore-errors-rror-test
  (is (= nil (ignore-errors (/ 0 0) (+ 3 4)))))

(deftest cl-find-test
  (is (= 3 (cl-find 3 '(1 2 3 4))))

  ;; :key
  (is (= 4 (cl-find 2 '(1 2 3 4) :key (fn [x] (/ x 2)))))

  ;; :test
  (is (=  "zabars" (cl-find "bar" '("joe" "went" "to" "zabars") :test (fn [a b] (.contains a b)))))

  )

(deftest underscore->camelcase-test
  (is (= (underscore->camelcase "foo") "Foo"))
  (is (= (underscore->camelcase "foo_bar") "FooBar")))

(deftest error-handling-fn-test
  (let [ed (error-handling-fn /)]
    (is (= '(true 2/3) (ed 2 3)))
    (is (= '(false "Caught exception: java.lang.ArithmeticException: Divide by zero") (ed 2 0)))))
        
(deftest map-invert-multiple-test
  (is (= {} (map-invert-multiple {})))
  (is (= '{1 #{:a}} (map-invert-multiple {:a 1})))
  (is (= '{1 #{:a} :x #{:b} :y #{:b}} (map-invert-multiple {:a 1 :b '(:x :y)})))
  (is (= '{1 #{:a :b} 2 #{:b} } (map-invert-multiple {:a 1 :b '(1 2)})))
  )

(deftest partition-lossless-test
  (is (= '((a b c) (d)) (partition-lossless 3 '(a b c d))))
  (is (= '((a b c)) (partition-lossless 3 '(a b c))))
  (is (= '((a b c)) (partition-lossless 10 '(a b c))))
  (is (= '() (partition-lossless 3 '()))))

(deftest map-chunked-test
  (let [f (fn [x] (* x 2))]
    (= (map f (range 100))
       (map-chunked #(map f %) 7 (range 100)))))
      
(deftest positions-test
  (is (= '(0 2 4 6 8) (positions even? '(0 1 2 3 4 3 2 1 0))))
  (is (= '(3 5) (positions= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest partition-lossless-test
  (= '((0 1 2) (3 4 5) (6 7 8) (9))
     (partition-lossless 3 (range 10))))

