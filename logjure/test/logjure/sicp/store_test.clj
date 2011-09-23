(ns logjure.sicp.store-test
  (:use 
    logjure.sicp.table
    logjure.sicp.store
    clojure.contrib.test-is
    )
  )

(deftest test-store-assertion-in-all
  (reset! *store* {})
  (is (= '{(all-assertions assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))} 
         (do (store-assertion-in-all '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-store-assertion-in-index
  (reset! *store* {})
  (is (= '{(address assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))} 
         (do (store-assertion-in-index '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-add-assertion!
  (reset! *store* {})
  (is (= '{(address assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
           (all-assertions assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
           } 
         (do (add-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
  (is (= '{(? assertion-stream) (((? x) (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
           (all-assertions assertion-stream) (((? x) (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
           } 
         (do (add-assertion! '((? x) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
  (is (= '{(all-assertions assertion-stream) ((((? x) (? y)) (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))} 
         (do (add-assertion! '(((? x) (? y)) (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) @*store*)))
  (reset! *store* {})
)

(deftest test-get-all-assertions
  (reset! *store* '{(all-assertions assertion-stream) ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))})
  (is (= '((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))) (get-all-assertions)))
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

(run-tests)
