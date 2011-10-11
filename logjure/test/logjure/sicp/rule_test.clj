(ns logjure.sicp.rule-test
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

(refer-private 'logjure.sicp.rule)

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

;initial frame is 'failed
(deftest test-unify-match-when-frame-failed
  (is (= 'failed (unify-match nil nil 'failed) ))
  (is (= 'failed (unify-match nil :x 'failed) ))
  (is (= 'failed (unify-match :x nil 'failed) ))
  (is (= 'failed (unify-match '?x nil 'failed) ))
  (is (= 'failed (unify-match nil '?x 'failed) ))
  (is (= 'failed (unify-match '?x :a 'failed) ))
  (is (= 'failed (unify-match :a '?x 'failed) ))
  (is (= 'failed (unify-match '?x '?x 'failed) ))
  (is (= 'failed (unify-match '?x '?y 'failed) ))
)

;initial frame is empty
(deftest test-unify-match-when-frame-empty
  (is (= 'failed (unify-match nil nil (make-empty-frame)) ))
  (is (= 'failed (unify-match nil :x (make-empty-frame)) ))
  (is (= 'failed (unify-match :x nil (make-empty-frame)) ))
  (is (= 'failed (unify-match '?x nil (make-empty-frame)) ))
  (is (= 'failed (unify-match nil '?x (make-empty-frame)) ))
  (is (= (make-empty-frame) (unify-match :x :x (make-empty-frame)) ))
  (is (= :x (get-value-in-frame '?x (unify-match '?x :x (make-empty-frame)))))
  (is (= :x (get-value-in-frame '?x (unify-match :x '?x (make-empty-frame)))))
)

;initial frame is not empty
(deftest test-unify-match-when-frame-not-empty
  ;no value for ?x in initial frame
  (is (= :x (get-value-in-frame '?x (unify-match '?x :x (extend-frame '?v :v (make-empty-frame))))))
  ;initial frame already contains variable value, and it is different, no match
  (is (= 'failed (unify-match '?x :X (extend-frame '?x :x (make-empty-frame)))))
  ;initial frame already contains variable value, and it is the same, match
  (is (= :x (get-value-in-frame '?x (unify-match '?x :x (extend-frame '?x :x (make-empty-frame))))))
  ;no value for ?x in initial frame, value is a list
  (is (= '(:x1 :x2) (get-value-in-frame '?x (unify-match '?x '(:x1 :x2) (extend-frame '?v :v (make-empty-frame))))))
  ;initial frame already contains variable value, and it is the same, both values are lists, match
  (is (= '(:x1 :x2) (get-value-in-frame '?x (unify-match '?x '(:x1 :x2) (extend-frame '?x '(:x1 :x2) (make-empty-frame))))))
)

;initial frame is empty
;pattern-and-datum-are-both-sequences
(deftest test-unify-match-when-frame-empty-and-pattern-and-datum-are-both-sequences
  ;initial frame is empty, successful match
  (is (= :b (get-value-in-frame '?x (unify-match '(?x :a) '(:b :a) (make-empty-frame)))))
  ;initial frame is empty, no match
  (is (= 'failed (unify-match '(?x :a) '(:b :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match
  (is (= :c (get-value-in-frame '?x (unify-match '((?x :a) :b) '((:c :a) :b) (make-empty-frame)))))
  ;initial frame is empty, 2 level nested, no match
  (is (= 'failed (unify-match '((?x :a) :b) '((:c :W) :b) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, no match
  (is (= 'failed (unify-match '((?x :a) :b) '((:c :a) :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match, variable on second pos
  (is (= :c (get-value-in-frame '?x (unify-match '((:a ?x) :b) '((:a :c) :b) (make-empty-frame)))))
  ;initial frame is empty, 2 level nested, no match, variable on second pos
  (is (= 'failed (unify-match '((:a ?x) :b) '((:W :c) :b) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, no match, variable on second pos
  (is (= 'failed (unify-match '((:a ?x) :b) '((:c :a) :W) (make-empty-frame))))
  ;initial frame is empty, 2 level nested, successful match, multiple vars
  (is (= :x (get-value-in-frame '?x (unify-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
  (is (= :y (get-value-in-frame '?y (unify-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
  (is (= :z (get-value-in-frame '?z (unify-match '((:a ?x ?y ?z) :b) '((:a :x :y :z) :b) (make-empty-frame)))))
)

;initial frame is not empty
;pattern-and-datum-are-both-sequences
(deftest test-unify-match-when-frame-not-empty-and-pattern-and-datum-are-both-sequences
  ;two variables, initial frame has value for one variable, value in frame is the same as the one being matched, match
  (is (= :x (get-value-in-frame '?x (unify-match '(?x ?y) '(:x :y) (extend-frame '?y :y (make-empty-frame))))))
  (is (= :y (get-value-in-frame '?y (unify-match '(?x ?y) '(:x :y) (extend-frame '?y :y (make-empty-frame))))))
  ;two variables, initial frame has value for both variables, values in frame are the same as the ones being matched, match
  (is (= :x (get-value-in-frame '?x (unify-match '(?x ?y) '(:x :y) (extend-frame '?x :x (extend-frame '?y :y (make-empty-frame)))))))
  (is (= :y (get-value-in-frame '?y (unify-match '(?x ?y) '(:x :y) (extend-frame '?x :x (extend-frame '?y :y (make-empty-frame)))))))
)

;test with not empty frame
(deftest test-unify-match-when-frame-has-binding-for-another-variable
  ;initial frame is not empty, value is a list, match
  (is (= '(f ?y) (get-value-in-frame '?x (unify-match '?x '(f b) (extend-frame '?x '(f ?y) (make-empty-frame))))))
  (is (= 'b (get-value-in-frame '?y (unify-match '?x '(f b) (extend-frame '?x '(f ?y) (make-empty-frame))))))
  ;initial frame is not empty, value is a list, no match, 
  ;because ?y is already bound to 'c and can not be bound to 'b
  (is (= 'failed (unify-match '?x '(f b) (extend-frame '?x '(f ?y) (extend-frame '?y 'c (make-empty-frame))))))
)

;test deep nested
(deftest test-unify-match-when-deep-nested
  ;initial frame is not empty, value is a list, match
  (is (= 'b (get-value-in-frame '?x (unify-match (deeply-nested 10000 '?x) (deeply-nested 10000 'b) (make-empty-frame)))))
)

(run-tests)
