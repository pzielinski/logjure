(ns logjure.sicp.store-test
  (:use 
    logjure.sicp.syntax
    logjure.sicp.table
    logjure.sicp.store
    logjure.utils.testing
    clojure.contrib.test-is
    )
  )

(refer-private 'logjure.sicp.store)

(deftest test-store-assertion-in-all
  (clear-table)
  (is (= '{(all-assertions assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (store-assertion-in-all '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (clear-table)
)

(deftest test-store-assertion-in-index
  (clear-table)
  (is (= '{(address assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (store-assertion-in-index '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (clear-table)
)

(deftest test-add-assertion!
  ;indexed by constant symbol
  (clear-table)
  (is (= '{(address assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  ;indexed by ?
  (clear-table)
  (is (= '{(? assertion-stream) #{(?x (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '(?x (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  ;not indexed, stored in all
  (clear-table)
  (is (= '{(all-assertions assertion-stream) #{((?x ?y) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '((?x ?y) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (clear-table)
)

(deftest test-get-all-assertions
  (reset! *store* '{(all-assertions assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}})
  (is (= '#{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))} (get-all-assertions)))
  (clear-table)
)

(deftest test-get-indexed-assertions
  (reset! *store* '{(address assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}})
  (is (= '#{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))} (get-indexed-assertions '(address ?x ?y))))
  (reset! *store* '{(address assertion-stream) 
                    #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
                      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))}})
  (is (= '#{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
            (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))} 
         (get-indexed-assertions '(address ?x ?y))))
  (clear-table)
)


(deftest test-store-rule-in-all
  (clear-table)
  (is (= '{(all-rules rule-stream) #{(rule (same ?x ?x))}} 
         (do (store-rule-in-all '(rule (same ?x ?x))) @*store*)))
  (clear-table)
)

(deftest test-store-rule-in-index
  (clear-table)
  (is (= '{(same rule-stream) #{(rule (same ?x ?x))}} 
         (do (store-rule-in-index '(rule (same ?x ?x))) @*store*)))
  (clear-table)
)

(deftest test-add-rule!
  ;indexed by constant symbol
  (clear-table)
  (is (= '{(same rule-stream) #{(rule (same ?x ?x))}} 
         (do (add-rule! '(rule (same ?x ?x))) @*store*)))
  ;indexed by constant symbol - multiple
  (clear-table)
  (is (= '{(append-to-form rule-stream) 
           #{(rule (append-to-form () ?y ?y))
             (rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))}} 
         (do 
           (add-rule! '(rule (append-to-form () ?y ?y))) 
           (add-rule! '(rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))) 
           @*store*)))
  ;indexed by ?
  (clear-table)
  (is (= '{(? rule-stream) #{(rule (?y ?x ?x))}} 
         (do (add-rule! '(rule (?y ?x ?x))) @*store*)))
  ;not indexed, stored in all
  (clear-table)
  (is (= '{(all-rules rule-stream) #{(rule ((?y ?y) ?x ?x))}} 
         (do (add-rule! '(rule ((?y ?y) ?x ?x))) @*store*)))
  (clear-table)
)

(deftest test-get-all-rules
  (reset! *store* '{(all-rules rule-stream) #{(rule (same ?x ?x))}})
  (is (= '#{(rule (same ?x ?x))} (get-all-rules)))
  (clear-table)
)

(deftest test-get-indexed-rules
  (reset! *store* '{(append-to-form rule-stream) 
           #{(rule (append-to-form () ?y ?y))
             (rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))}})
  (is (= '#{(rule (append-to-form () ?y ?y))
            (rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))}
         (get-indexed-rules '(append-to-form (? a) (? b) (? c)))))
  (clear-table)
)

(deftest test-add-to-stream
  (is (= '#{:x} (add-to-stream :x #{})))
  (is (= '#{:x :y} (add-to-stream :y #{:x})))
  )

(deftest test-use-index?
  (is (= true (use-index? '(same ?x ?y))))
  (is (= false (use-index? '(?x ?y))))
  )

(deftest test-indexable?
  (is (= true (indexable? '(same ?x ?y))))
  (is (= true (indexable? '(?x ?y))))
  )


(run-tests)
