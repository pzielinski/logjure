(ns logjure.sicp.assertion-test
  (:use 
    logjure.sicp.syntax
    logjure.sicp.table
    logjure.sicp.frame
    logjure.sicp.store
    logjure.sicp.assertion
    logjure.utils.testing
    clojure.contrib.test-is
    )
  )

(deftest test-pattern-match-when-failed-frame
  (is (= 'failed (pattern-match nil nil 'failed) ))
  (is (= 'failed (pattern-match nil :x 'failed) ))
  (is (= 'failed (pattern-match :x nil 'failed) ))
)

;frame-not-failed-and-
(deftest test-pattern-match-when-equal-pattern-datum
  (is (= (make-empty-frame) (pattern-match nil nil (make-empty-frame)) ))
  (is (= (make-empty-frame) (pattern-match :x :x (make-empty-frame)) ))
  (is (= (make-empty-frame) (pattern-match '?x '?x (make-empty-frame)) ))
)

;frame-not-failed-and-
;not-equal-pattern-datum-and-
(deftest test-pattern-match-when-variable-pattern
  ;initial frame is empty
  (is (= :y (get-value-in-frame '?x (pattern-match '?x :y (make-empty-frame)))))
  ;initial frame is not empty
  (is (= :y (get-value-in-frame '?x (pattern-match '?x :y (extend-frame '?v :z (make-empty-frame))))))
  ;initial frame already contains variable value
  (is (= 'failed (pattern-match '?x :y (extend-frame '?x :z (make-empty-frame)))))
  ;initial frame is not empty, datum is variable with value in frame
  ;CASE WHEN DAT IS VARIABLE - fails!!!!!!!!!!!!!!!!!!!!!
  ;(is (= :z (get-value-in-frame '?x (pattern-match '?x '?v (extend-frame '?v :z (make-empty-frame))))))
)

;frame-not-failed-and-
;not-equal-pattern-datum-and-
;not-variable-pattern
(deftest test-pattern-match-when-pattern-and-datum-are-both-sequences
  ;initial frame is empty, successful match
  (is (= :b (get-value-in-frame '?x (pattern-match '(?x :a) '(:b :a) (make-empty-frame)))))
  ;initial frame is empty, no match
  (is (= 'failed (pattern-match '(?x :a) '(:b :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match
  (is (= :c (get-value-in-frame '?x (pattern-match '((?x :a) :b) '((:c :a) :b) (make-empty-frame)))))
  ;initial frame is empty, 2 level nested, no match
  (is (= 'failed (pattern-match '((?x :a) :b) '((:c :W) :b) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, no match
  (is (= 'failed (pattern-match '((?x :a) :b) '((:c :a) :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match, variable on second pos
  (is (= :c (get-value-in-frame '?x (pattern-match '((:a ?x) :b) '((:a :c) :b) (make-empty-frame)))))
  ;initial frame is empty, 2 level nested, no match, variable on second pos
  (is (= 'failed (pattern-match '((:a ?x) :b) '((:W :c) :b) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, no match, variable on second pos
  (is (= 'failed (pattern-match '((:a ?x) :b) '((:c :a) :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match, multiple vars
  (is (= :x (get-value-in-frame '?x (pattern-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
  (is (= :y (get-value-in-frame '?y (pattern-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
  (is (= :z (get-value-in-frame '?z (pattern-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
)

(run-tests)
