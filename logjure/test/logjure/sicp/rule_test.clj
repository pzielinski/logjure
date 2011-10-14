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

(deftest test-unify-match-seq
  ;failed frame
  (is (= '([nil nil failed]) (unify-match-seq nil nil 'failed) ))
  (is (= '([nil :x failed]) (unify-match-seq nil :x 'failed) ))
  (is (= '([:x nil failed]) (unify-match-seq :x nil 'failed) ))
  (is (= '([?x nil failed]) (unify-match-seq '?x nil 'failed) ))
  (is (= '([nil ?x failed]) (unify-match-seq nil '?x 'failed) ))
  (is (= '([?x :a failed]) (unify-match-seq '?x :a 'failed) ))
  (is (= '([:a ?x failed]) (unify-match-seq :a '?x 'failed) ))
  (is (= '([?x (:a) failed]) (unify-match-seq '?x '(:a) 'failed) ))
  (is (= '([(:a) ?x failed]) (unify-match-seq '(:a) '?x 'failed) ))
  (is (= '([?x ?x failed]) (unify-match-seq '?x '?x 'failed) ))
  (is (= '([?x (?x) failed]) (unify-match-seq '?x '(?x) 'failed) ))
  (is (= '([(?x) ?x failed]) (unify-match-seq '(?x) '?x 'failed) ))
  (is (= '([?x ?y failed]) (unify-match-seq '?x '?y 'failed) ))
  (is (= '([?x (?y) failed]) (unify-match-seq '?x '(?y) 'failed) ))
  (is (= '([(?x) ?y failed]) (unify-match-seq '(?x) '?y 'failed) ))
  (is (= '([(?x) (?y) failed]) (unify-match-seq '(?x) '(?y) 'failed) ))
  ;empty frame
  (is (= '([nil nil failed]) 
         (unify-match-seq nil nil (make-empty-frame)) ))
  (is (= '([nil :x failed]) 
         (unify-match-seq nil :x (make-empty-frame)) ))
  (is (= '([:x nil failed]) 
         (unify-match-seq :x nil (make-empty-frame)) ))
  (is (= '([?x nil failed]) 
         (unify-match-seq '?x nil (make-empty-frame)) ))
  (is (= '([nil ?x failed]) 
         (unify-match-seq nil '?x (make-empty-frame)) ))
  (is (= '([(?x) nil failed]) 
         (unify-match-seq '(?x) nil (make-empty-frame)) ))
  (is (= '([nil (?x) failed]) 
         (unify-match-seq nil '(?x) (make-empty-frame)) ))
  (is (= (list [:x :x (map2frame {})]) 
         (unify-match-seq :x :x (make-empty-frame)) ))
  (is (= (list ['?x :x (map2frame {'?x :x})]) 
         (unify-match-seq '?x :x (make-empty-frame))))
  (is (= (list [:x '?x (map2frame {'?x :x})]) 
         (unify-match-seq :x '?x (make-empty-frame))))
  (is (= (list ['?x '(?y) (map2frame {'?x '(?y)})]) 
         (unify-match-seq '?x '(?y) (make-empty-frame))))
  (is (= (list ['(?x) :x (map2frame {})]) 
         (unify-match-seq '(?x) :x (make-empty-frame))))
  )

(deftest test-unify-match-seq-p1-is-variable-and-p2-is-not
  ;failed frame
  (is (= '([?x :a failed]) 
         (unify-match-seq '?x :a 'failed) ))
  ;invalid p2 arg
  (is (= '([?x nil failed]) 
         (unify-match-seq '?x nil (make-empty-frame)) ))
  ;no p1 value in frame & p2 does not depend on p1 variable
  (is (= (list ['?x :x (map2frame {'?x :x})]) 
         (unify-match-seq '?x :x (make-empty-frame))))
  ;no p1 value in frame & p2 does not depend on p1 variable
  (is (= (list ['?x '(((:x))) (map2frame {'?x '(((:x)))})]) 
         (unify-match-seq '?x '(((:x))) (make-empty-frame))))
  ;no p1 value in frame & p2 does not depend on p1 variable - p2 is an expression
  (is (= (list ['?x '(((?y))) (map2frame {'?x '(((?y)))})]) 
         (unify-match-seq '?x '(((?y))) (make-empty-frame))))
  ;no p1 value in frame & p2 does depend on p1 variable - p2 is an expression
  (is (= (list ['?x '(((((?x))))) 'failed]) 
         (unify-match-seq '?x '(((((?x))))) (make-empty-frame))))
  ;there is p1 value in frame & its different & unify-mach will return 'failed
  (is (= (list ['?x :x (map2frame {'?x :a})] [:a :x (map2frame {'?x :a})]) 
         (unify-match-seq '?x :x (map2frame {'?x :a}))))
  )

(deftest test-unify-match-seq-p1-is-variable-and-p2-is-variable
  ;failed frame
  (is (= '([?x ?y failed]) 
         (unify-match-seq '?x '?y 'failed) ))
  ;no p1 value in frame & p2 does not depend on p1 variable
  (is (= (list ['?x '?y (map2frame {'?x '?y})]) 
         (unify-match-seq '?x '?y (make-empty-frame))))
  ;no p1 value in frame & p2 does depend on p1 variable
  (is (= (list ['?x '?x 'failed]) 
         (unify-match-seq '?x '?x (make-empty-frame))))
  ;there is p1 value in frame - match
  (is (= (list ['?x '?y (map2frame {'?x :a})] [:a '?y (map2frame {'?x :a '?y :a})]) 
         (unify-match-seq '?x '?y (map2frame {'?x :a}))))
  ;there is p2 value in frame - match - now fully resolved - LESS FRAMES THAN ABOVE!!!
  (is (= (list ['?x '?y (map2frame {'?y :a '?x :a})]) 
         (unify-match-seq '?x '?y (map2frame {'?y :a}))))
  ;there is p1 value in frame - p2 is expr = ?x is bound to expr that is not fully resolved
  (is (= '([?x (?y :a) {?x (:b ?z)}] [:b ?y {?y :b, ?x (:b ?z)}] [?z :a {?z :a, ?y :b, ?x (:b ?z)}]) 
         (unify-match-seq '?x '(?y :a) (map2frame {'?x '(:b ?z)}))))
  )

(deftest test-unify-match-seq-p2-is-variable-and-p1-is-not
  ;failed frame
  (is (= '([:a ?x failed]) 
         (unify-match-seq :a '?x 'failed) ))
  ;invalid p1 arg
  (is (= '([nil ?x failed]) 
         (unify-match-seq nil '?x (make-empty-frame)) ))
  ;no p2 value in frame & p1 does not depend on p2 variable
  (is (= (list [:x '?x (map2frame {'?x :x})]) 
         (unify-match-seq :x '?x (make-empty-frame))))
  ;no p2 value in frame & p1 does depend on p2 variable
  (is (= (list ['?x '?x 'failed]) 
         (unify-match-seq '?x '?x (make-empty-frame))))
  ;no p2 value in frame & p1 does depend on p2 variable - p1 is an expression
  (is (= (list ['(((((?x))))) '?x 'failed]) 
         (unify-match-seq '(((((?x))))) '?x (make-empty-frame))))
  ;there is p2 value in frame & its different & unify-mach will return 'failed
  (is (= (list [:x '?x (map2frame {'?x :a})] [:x :a (map2frame {'?x :a})]) 
         (unify-match-seq :x '?x (map2frame {'?x :a}))))
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
    ;(is (= true (depends-on? (deeply-nested 10000 '?x) '?x (make-empty-frame))))
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
  (is (= 'failed (unify-match '?x '(:a) 'failed) ))
  (is (= 'failed (unify-match '(:a) '?x 'failed) ))
  (is (= 'failed (unify-match '?x '?x 'failed) ))
  (is (= 'failed (unify-match '?x '(?x) 'failed) ))
  (is (= 'failed (unify-match '(?x) '?x 'failed) ))
  (is (= 'failed (unify-match '?x '?y 'failed) ))
  (is (= 'failed (unify-match '?x '(?y) 'failed) ))
  (is (= 'failed (unify-match '(?x) '?y 'failed) ))
  (is (= 'failed (unify-match '(?x) '(?y) 'failed) ))
)

;initial frame is empty
(deftest test-unify-match-when-frame-empty
  (is (= 'failed (unify-match nil nil (make-empty-frame)) ))
  (is (= 'failed (unify-match nil :x (make-empty-frame)) ))
  (is (= 'failed (unify-match :x nil (make-empty-frame)) ))
  (is (= 'failed (unify-match '?x nil (make-empty-frame)) ))
  (is (= 'failed (unify-match nil '?x (make-empty-frame)) ))
  (is (= 'failed (unify-match '(?x) nil (make-empty-frame)) ))
  (is (= 'failed (unify-match nil '(?x) (make-empty-frame)) ))
  (is (= (make-empty-frame) (unify-match :x :x (make-empty-frame)) ))
  (is (= :x (get-value-in-frame '?x (unify-match '?x :x (make-empty-frame)))))
  (is (= :x (get-value-in-frame '?x (unify-match :x '?x (make-empty-frame)))))
  (is (= '(?y) (get-value-in-frame '?x (unify-match '?x '(?y) (make-empty-frame)))))
  (is (= 'failed (unify-match '(?x) :x (make-empty-frame))))
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
  ;(is (= 'b (get-value-in-frame '?x (unify-match (deeply-nested 10000 '?x) (deeply-nested 10000 'b) (make-empty-frame)))))
)

(deftest test-replace-symbol
  (is (= :deepest (last (tree-seq-depth (replace-symbol '((:bottom)) :bottom :deepest)))))
  ;(is (= :deepest (last (tree-seq-depth (replace-symbol (deeply-nested 10000) :bottom :deepest)))))
  )

(deftest test-resolve-variables
  (is (= '(:x) (resolve-variables '(:x) (map2frame {}))))
  (is (= '(?x) (resolve-variables '(?x) (map2frame {}))))
  (is (= '(:x) (resolve-variables '(?x) (map2frame {'?x :x}))))
  (is (= '((:x)(:x)) (resolve-variables '((?x)(?x)) (map2frame {'?x :x}))))
  (is (= '((:x)(((:x) :x (:y)))) (resolve-variables '((?x)(((?x) ?x (?y)))) (map2frame {'?x :x '?y :y}))))
  (is (= '((:y)(:y)) (resolve-variables '((?x)(?x)) (map2frame {'?x '?y '?y :y}))))
  ;(is (= :x (last (tree-seq-depth (resolve-variables (deeply-nested 10000 '?x) (map2frame {'?x :x}))))))
  (let 
    ;{... ?x9 ?x10, ?x10 ?x11, ?x5 ?x6, ?x6 ?x7, ?x7 ?x8, ?x8 ?x9, ?x4 ?x5, ?x3 ?x4, ?x2 ?x3, ?x1 ?x2}
    [frame 
     (map2frame 
       (assoc 
         (apply 
           assoc 
           {} 
           (apply concat (take 10000 (partition 2 1 (map #(symbol (str '?x %)) (iterate inc 1))))))
         '?x10000
         :x))]
    (is (= :x (resolve-variables '?x1 frame))))
  )

(deftest test-normalize-frame
  (is (= (map2frame {'?x :b '?y :b}) 
         (normalize-frame (map2frame {'?x '?y '?y :b}))))
  (is (= (map2frame {'?z :a '?y :b '?x '(:b :a)}) 
         (normalize-frame (map2frame {'?z :a '?y :b '?x '(:b ?z)}))))
  ;even in one pass of normalize-frame ?x is resolved to :b because of recur in resolve-variables
  (is (= (map2frame {'?x :b '?z :b '?y :b}) 
         (normalize-frame (map2frame {'?x '?z '?z '?y '?y :b}))))
  ;it also works here
  (is (= (map2frame {'?x '(:b :c) '?z :b '?y :b}) 
         (normalize-frame (map2frame {'?x '(?z :c) '?z '?y '?y :b}))))
  ;no so here (need normalize-frame-fully)
  (is (= (map2frame {'?x '((?y :d) :c) '?z '(:b :d) '?y :b}) 
         (normalize-frame (map2frame {'?x '(?z :c) '?z '(?y :d) '?y :b}))))
  )

(deftest test-normalize-frame-fully
  (is (= (map2frame {'?x :b '?y :b}) 
         (normalize-frame-fully (map2frame {'?x '?y '?y :b}))))
  (is (= (map2frame {'?z :a '?y :b '?x '(:b :a)}) 
         (normalize-frame-fully (map2frame {'?z :a '?y :b '?x '(:b ?z)}))))
  (is (= (map2frame {'?x :b '?z :b '?y :b}) 
         (normalize-frame-fully (map2frame {'?x '?z '?z '?y '?y :b}))))
  (is (= (map2frame {'?x '(:b :c) '?z :b '?y :b}) 
         (normalize-frame-fully (map2frame {'?x '(?z :c) '?z '?y '?y :b}))))
  ;normalize-frame-fully works
  (is (= (map2frame {'?x '((:b :d) :c) '?z '(:b :d) '?y :b}) 
         (normalize-frame-fully (map2frame {'?x '(?z :c) '?z '(?y :d) '?y :b}))))
  )

(run-tests)
