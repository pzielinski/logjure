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

;initial frame is 'failed
(deftest test-pattern-match-when-frame-failed
  (is (= 'failed (pattern-match nil nil 'failed) ))
  (is (= 'failed (pattern-match nil :x 'failed) ))
  (is (= 'failed (pattern-match :x nil 'failed) ))
)

;initial frame is empty
(deftest test-pattern-match-when-frame-empty
  (is (= (make-empty-frame) (pattern-match nil nil (make-empty-frame)) ))
  (is (= (make-empty-frame) (pattern-match :x :x (make-empty-frame)) ))
  (is (= :x (get-value-in-frame '?x (pattern-match '?x :x (make-empty-frame)))))
)

;initial frame is not empty
;CASE WHEN DATUM IS VARIABLE - fails! this is implemented by unify-match in rule!
(deftest test-pattern-match-when-frame-not-empty
  ;no value for ?x in initial frame
  (is (= :x (get-value-in-frame '?x (pattern-match '?x :x (extend-frame '?v :v (make-empty-frame))))))
  ;initial frame already contains variable value, and it is different, no match
  (is (= 'failed (pattern-match '?x :X (extend-frame '?x :x (make-empty-frame)))))
  ;initial frame already contains variable value, and it is the same, match
  (is (= :x (get-value-in-frame '?x (pattern-match '?x :x (extend-frame '?x :x (make-empty-frame))))))
  ;no value for ?x in initial frame, value is a list
  (is (= '(:x1 :x2) (get-value-in-frame '?x (pattern-match '?x '(:x1 :x2) (extend-frame '?v :v (make-empty-frame))))))
  ;initial frame already contains variable value, and it is the same, both values are lists, match
  (is (= '(:x1 :x2) (get-value-in-frame '?x (pattern-match '?x '(:x1 :x2) (extend-frame '?x '(:x1 :x2) (make-empty-frame))))))
)

;initial frame is empty
;pattern-and-datum-are-both-sequences
(deftest test-pattern-match-when-frame-empty-and-pattern-and-datum-are-both-sequences
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

;initial frame is not empty
;pattern-and-datum-are-both-sequences
(deftest test-pattern-match-when-frame-not-empty-and-pattern-and-datum-are-both-sequences
  ;two variables, initial frame has value for one variable, value in frame is the same as the one being matched, match
  (is (= :x (get-value-in-frame '?x (pattern-match '(?x ?y) '(:x :y) (extend-frame '?y :y (make-empty-frame))))))
  (is (= :y (get-value-in-frame '?y (pattern-match '(?x ?y) '(:x :y) (extend-frame '?y :y (make-empty-frame))))))
  ;two variables, initial frame has value for both variables, values in frame are the same as the ones being matched, match
  (is (= :x (get-value-in-frame '?x (pattern-match '(?x ?y) '(:x :y) (extend-frame '?x :x (extend-frame '?y :y (make-empty-frame)))))))
  (is (= :y (get-value-in-frame '?y (pattern-match '(?x ?y) '(:x :y) (extend-frame '?x :x (extend-frame '?y :y (make-empty-frame)))))))
)

;test with not empty frame
(deftest test-pattern-match-when-frame-has-binding-for-another-variable
  ;initial frame is not empty, value is a list, match
  (is (= '(f ?y) (get-value-in-frame '?x (pattern-match '?x '(f b) (extend-frame '?x '(f ?y) (make-empty-frame))))))
  (is (= 'b (get-value-in-frame '?y (pattern-match '?x '(f b) (extend-frame '?x '(f ?y) (make-empty-frame))))))
  ;initial frame is not empty, value is a list, no match, 
  ;because ?y is already bound to 'c and can not be bound to 'b
  (is (= 'failed (pattern-match '?x '(f b) (extend-frame '?x '(f ?y) (extend-frame '?y 'c (make-empty-frame))))))
)

;test deep nested
(deftest test-pattern-match-when-deep-nested
  ;initial frame is not empty, value is a list, match
  (is (= 'b (get-value-in-frame '?x (pattern-match (deeply-nested 100 '?x) (deeply-nested 100 'b) (make-empty-frame)))))
)

(run-tests)
