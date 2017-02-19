(ns mtu.test.core
  (:use clojure.test)
  (:use mtu.core))

(deftest cl-find-test
  (is (= 3 (cl-find 3 '(1 2 3 4))))
  (testing ":key arg"
    (is (= 4 (cl-find 2 '(1 2 3 4) :key (fn [x] (/ x 2))))))
  (testing ":test arg"
    (is (=  "zabars" (cl-find "bar" '("joe" "went" "to" "zabars") :test (fn [a b] (.contains a b)))))))

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

(deftest powerset-test
  (is (= #{#{} #{3} #{2} #{1} #{1 3 2} #{1 3} #{1 2} #{3 2}}
         (powerset #{1 2 3})))
  (testing "works on empty set"
    (is (= #{#{}} (powerset #{}))))
  (testing "works on lists"
    (is (= '#{#{a} #{c b} #{a b} #{} #{b} #{c} #{a c} #{a c b}}
           (powerset '(a b c)))))
  (testing "works on decent sized list"
    (= (Math/round (Math/pow 2 18))
       (count (powerset (range 18))))))

(deftest compare-tests
  (is (>* 2 1))
  (is (>* "foo" "bar"))
  (is (not (>* 1 1)))
  (is (<* 2 3))
  (is (<* "foo" "quux"))
  (is (not (<* 1 1))))

(deftest maxby-minby-test
  (= "tediously"
     (max-by count (tokens "this is a tediously long string")))
  (= "a"
     (min-by count (tokens "this is a tediously long string"))))

(defn rcons [a b] (cons b a))

(deftest doseq*-test
  (let [acc (atom '())]
    (doseq* [a '(1 2 3)
             b '(x y z)]
            (swap! acc rcons (list a b)))
    (= '((1 x) (2 y) (3 z))
       (reverse @acc)))
  ;; TODO seqs of different length
  )

        

