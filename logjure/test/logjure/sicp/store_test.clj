(ns logjure.sicp.store-test
  (:use 
    logjure.sicp.syntax
    logjure.sicp.table
    logjure.sicp.store
    clojure.contrib.test-is
    )
  )

(deftest test-store-assertion-in-all
  (reset! *store* {})
  (is (= '{(all-assertions assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (store-assertion-in-all '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-store-assertion-in-index
  (reset! *store* {})
  (is (= '{(address assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (store-assertion-in-index '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-add-assertion!
  ;indexed by constant symbol
  (reset! *store* {})
  (is (= '{(address assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  ;indexed by ?
  (reset! *store* {})
  (is (= '{(? assertion-stream) #{((? x) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '((? x) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  ;not indexed, stored in all
  (reset! *store* {})
  (is (= '{(all-assertions assertion-stream) #{(((? x) (? y)) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}} 
         (do (add-assertion! '(((? x) (? y)) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-get-all-assertions
  (reset! *store* '{(all-assertions assertion-stream) #{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))}})
  (is (= '#{(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))} (get-all-assertions)))
  (reset! *store* {})
)

(deftest test-get-indexed-assertions
  (reset! *store* '{(address assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))})
  (is (= '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) (get-indexed-assertions '(address (? x) (? y)))))
  (reset! *store* '{(address assertion-stream) 
                    (
                      (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
                      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
                      )
                    })
  (is (= '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
            (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))) 
         (get-indexed-assertions '(address (? x) (? y)))))
  (reset! *store* {})
)


(deftest test-store-rule-in-all
  (reset! *store* {})
  (is (= '{(all-rules rule-stream) #{(rule (same (? x) (? x)))}} 
         (do (store-rule-in-all '(rule (same (? x) (? x)))) @*store*)))
  (reset! *store* {})
)

(deftest test-store-rule-in-index
  (reset! *store* {})
  (is (= '{(same rule-stream) #{(rule (same (? x) (? x)))}} 
         (do (store-rule-in-index '(rule (same (? x) (? x)))) @*store*)))
  (reset! *store* {})
)

(deftest test-add-rule!
  ;indexed by constant symbol
  (reset! *store* {})
  (is (= '{(same rule-stream) #{(rule (same (? x) (? x)))}} 
         (do (add-rule! '(rule (same (? x) (? x)))) @*store*)))
  ;indexed by constant symbol - multiple
  (reset! *store* {})
  (is (= '{(append-to-form rule-stream) 
           #{(rule (append-to-form () ?y ?y))
             (rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))}} 
         (do 
           (add-rule! '(rule (append-to-form () ?y ?y))) 
           (add-rule! '(rule (append-to-form (?u . ?v) ?y (?u . ?z)) (append-to-form ?v ?y ?z))) 
           @*store*)))
  ;indexed by ?
  (reset! *store* {})
  (is (= '{(? rule-stream) #{(rule ((? y) (? x) (? x)))}} 
         (do (add-rule! '(rule ((? y) (? x) (? x)))) @*store*)))
  ;not indexed, stored in all
  (reset! *store* {})
  (is (= '{(all-rules rule-stream) #{(rule (((? y) (? y)) (? x) (? x)))}} 
         (do (add-rule! '(rule (((? y) (? y)) (? x) (? x)))) @*store*)))
  (reset! *store* {})
)

(deftest test-get-all-rules
  (reset! *store* '{(all-rules rule-stream) ((rule (same (? x) (? x))))})
  (is (= '((rule (same (? x) (? x)))) (get-all-rules)))
  (reset! *store* {})
)

(deftest test-get-indexed-rules
  (reset! *store* {})
)


(run-tests)
