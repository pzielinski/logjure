(ns logjure.utils.sequtil-test
  (:use 
    logjure.utils.sequtil
    logjure.utils.testing 
    clojure.test
    )
)

(deftest test-deep-equal?
  (is (= false (deep-equal? (deeply-nested 10000 '?x) (deeply-nested 10000 'b))))
  (is (= true (deep-equal? (deeply-nested 10000 '?x) (deeply-nested 10000 '?x))))
)

(defn ^:dynamic inc
  [x] (clojure.core/inc x))

(deftest test-lazy-concat
  (is (= [1 []] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 0))))
  (is (= [2 [1]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 1))))
  (is (= [3 [1 2]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 2))))
  (is (= [4 [1 2 3]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 3))))
  (is (= [5 [1 2 3 4]] (recorder inc #(+ % 1) (nth (lazy-concat (map list (iterate inc 1))) 4))))
)

(run-tests)
