(ns logjure.sicp.table-test
  (:use 
    logjure.sicp.table
    clojure.contrib.test-is
    )
  )

(deftest test-clear-table
  (reset! *store* {'(1 2) "x"})
  (is (= {} (do (clear-table) @*store*)))
)

(deftest test-put
  (reset! *store* {})
  (is (= {'(1 2) "x"} (do (put "x" 1 2) @*store*)))
  (is (= {'(1 2) "x" '(1 2 3) "y"} (do (put "y" 1 2 3) @*store*)))
  (is (= {'(1 2) "z" '(1 2 3) "y"} (do (put "z" 1 2) @*store*)))
  (reset! *store* {})
)

(deftest test-get-from-table
  (reset! *store* {'(1 2) "x"})
  (is (= "x" (get-from-table 1 2)))
  (reset! *store* {'(1 2) "y" '(1 2 3) "z"})
  (is (= "y" (get-from-table 1 2)))
  (is (= "z" (get-from-table 1 2 3)))
  (is (= nil (get-from-table 1)))
  (is (= nil (get-from-table)))
  (reset! *store* {})
)

(run-tests)

