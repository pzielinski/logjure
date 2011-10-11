(ns logjure.sicp.rule-test)

(ns logjure.sicp.assertion-test
  (:use 
    logjure.sicp.syntax
    logjure.sicp.table
    logjure.sicp.frame
    logjure.sicp.store
    logjure.sicp.rule
    logjure.utils.treeseq
    logjure.utils.testing
    clojure.contrib.test-is
    )
  )

(deftest test-depends-on? ;[expr var frame]
    ;emtpy frame
    (is (= false (depends-on? :a :a (make-empty-frame))))
    (is (= false (depends-on? '?x :a (make-empty-frame))))
    (is (= false (depends-on? :a '?x (make-empty-frame))))
    (is (= true (depends-on? '?x '?x (make-empty-frame))))
    ;binding in frame
    (is (= true (depends-on? '?y '?x (extend-frame '?y '?x (make-empty-frame)))))
    ;list
    (is (= true (depends-on? '(?x) '?x (make-empty-frame))))
    (is (= true (depends-on? '((((?x)))) '?x (make-empty-frame))))
    (is (= false (depends-on? '((((:a)))) '?x (make-empty-frame))))
    (is (= false (depends-on? '((((:a)))) :a (make-empty-frame))))
    ;list & binding in frame
    (is (= true (depends-on? '((((?y)))) '?x (extend-frame '?y '?x (make-empty-frame)))))
    ;list & list binding in frame
    (is (= true (depends-on? '((((?y)))) '?x (extend-frame '?y '((?x)) (make-empty-frame)))))
    ;list & double list binding in frame
    (is (= true (depends-on? '((((?y)))) '?x (extend-frame '?y '((?z)) (extend-frame '?z '(?x) (make-empty-frame))))))
    ;deep nested
    (is (= true (depends-on? (deeply-nested 10000 '?x) '?x (make-empty-frame))))
  )

(run-tests)
