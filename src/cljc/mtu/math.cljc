(ns mtu.math)

(defn interpolate [a b s]
  (+ (* a (- 1 s)) (* b s)))

(defn interpolated [a b n]
  (map #(interpolate a b (/ % n)) (range n)))

(defn distance [x0 y0 x1 y1]
  (Math/sqrt (+ (Math/pow (- x0 x1) 2) (Math/pow (- y0 y1) 2))))

(defn rescale [val from-lower from-upper to-lower to-upper]
  (+ to-lower
     (* (- val from-lower)
        (/ (- to-upper to-lower)
           (- from-upper from-lower)))))

(defn r2d [r]
  (* r (/ 180 Math/PI)))

(defn d2r [d]
  (* d (/ Math/PI 180)))

;;; ⩇⩆⩇ Naive handy statistics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn mean "Return the arithmetic mean of the elements of `seq`"
  [seq]
  (/ (reduce + seq)
     (count seq)))

(defn standard-deviation "Return standard deviation of the elements of `seq`"
  [seq]
  (let [mean0 (mean seq)]
    (Math/sqrt
     (/ (reduce + (map #(Math/pow (- % mean0) 2) seq))
        (- (count seq) 1)))))

(defn score-by "Return a list of [elt score] pairs, in descending score order."
  [keyfn seq]
  (reverse
   (sort-by second
            (map (fn [elt] [elt (keyfn elt)]) seq))))

(defn outliers-by "Return elements of `seq` on whom `scorefn` is more than `factor` standard-deviations away from the mean."
  [scorefn seq factor]
  (let [scores (map scorefn seq)
        threshold (+ (mean scores) (* factor (standard-deviation scores)))]
    (filter identity (map (fn [elt score] (when (>= score threshold) elt)) seq scores))))

;;; A highly useful and underused statistic
(defn coefficent-of-variation "Return coefficent of variation of the elements of `seq`"
  [seq]
  (/ (standard-deviation seq)
     (mean seq)))

(defn iles [seq n]
  (let [sorted (into [] (sort seq))
        count (double (count seq))]
    (map #(nth sorted (Math/round (* % (/ count n))))
         (range 1 n))))
    
(defn geometric-mean "Return the geometric mean of the elements of `seq`"
  [seq]
  (Math/pow (reduce * (map double seq))
            (/ 1 (count seq))))

(def primes
  (cons 2
        (lazy-seq
         (filter
          (fn [n] (not (some (fn [i] (= 0 (rem n i)))
                             (take-while #(<= % (Math/sqrt n)) primes))))
          (iterate #(+ % 2) 3)
          ))))
