(ns logjure.utils.sequtil-test
  (:use 
    logjure.utils.sequtil
    logjure.utils.testing 
    clojure.test
    )
)

(defn ^:dynamic inc
  [x] (clojure.core/inc x))

(deftest test-lazy-concat
  (is (= '() (doall (lazy-concat '()))))
  (is (= '(:a) (doall (lazy-concat '((:a))))))
  (is (= '(:a :b) (doall (lazy-concat '((:a) (:b))))))
  (is (= '(:a :b (:c (:x)) :d) (doall (lazy-concat '((:a) (:b (:c (:x))) (:d))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-concat '(((:a :b (:c))) (:a :b (:c)) (:c))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-concat '(((:a :b (:c))) (:a :b (:c)) (:c) ())))))
  (is (= '(:a) (doall (lazy-concat '(() (:a))))))
  (is (= '(:a) (doall (lazy-concat '(() () (:a))))))
)

(deftest test-lazy-concat-laziness
  (is (= [1 []] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 0))))
  (is (= [2 [1]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 1))))
  (is (= [3 [1 2]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 2))))
  (is (= [4 [1 2 3]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 3))))
  (is (= [5 [1 2 3 4]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 4))))
  ;TEST StackOverflow
  (is (= 10001 (nth (lazy-concat (map list (iterate inc 1))) 10000)))
)

(deftest test-lazy-concat-map-laziness
  (is (= [1 []] (recorder inc #(+ % 1) (nth (lazy-concat-map list (iterate inc 1)) 0))))
  (is (= [2 [1]] (recorder inc #(+ % 1) (nth (lazy-concat-map list (iterate inc 1)) 1))))
  (is (= [3 [1 2]] (recorder inc #(+ % 1) (nth (lazy-concat-map list (iterate inc 1)) 2))))
  (is (= [4 [1 2 3]] (recorder inc #(+ % 1) (nth (lazy-concat-map list (iterate inc 1)) 3))))
  (is (= [5 [1 2 3 4]] (recorder inc #(+ % 1) (nth (lazy-concat-map list (iterate inc 1)) 4))))
  ;TEST StackOverflow
  (is (= 10001 (nth (lazy-concat-map list (iterate inc 1)) 10000)))
)

(run-tests)
