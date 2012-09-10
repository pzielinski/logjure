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

(run-tests)
