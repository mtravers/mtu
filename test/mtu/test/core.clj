(ns mtu.test.core
  (:use clojure.test)
  (:use mtu.core))


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

(deftest map-invert-multiple-test
  (is (= {} (map-invert-multiple {})))
  (is (= '{1 #{:a}} (map-invert-multiple {:a 1})))
  (is (= '{1 #{:a} :x #{:b} :y #{:b}} (map-invert-multiple {:a 1 :b '(:x :y)})))
  (is (= '{1 #{:a :b} 2 #{:b} } (map-invert-multiple {:a 1 :b '(1 2)})))
  )

(deftest map-chunked-test
  (let [f (fn [x] (* x 2))]
    (= (map f (range 100))
       (map-chunked #(map f %) 7 (range 100)))))
      
(deftest positions-test
  (is (= '(0 2 4 6 8) (positions even? '(0 1 2 3 4 3 2 1 0))))
  (is (= '(3 5) (positions= 3 '(0 1 2 3 4 3 2 1 0)))))




