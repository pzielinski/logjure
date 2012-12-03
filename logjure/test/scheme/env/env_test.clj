(ns scheme.env.env-test)

(use 'clojure.test)

(use 'scheme.env.env)

(deftest empty-environment?-test
  (is (= true (empty-environment? (the-empty-environment))))
  )

(deftest empty-environment?-false-test
  (is (= false (empty-environment? (set-variable-value-in-env :a 1 (the-empty-environment)))))
  )

(deftest set-variable-value-in-env-test
  (is (= 1 (lookup-variable-value-in-env :a (set-variable-value-in-env :a 1 (the-empty-environment)))))
  )

(deftest set-variable-value-in-env-two-1-test
  (is (= 1 (lookup-variable-value-in-env 
             :a 
             (set-variable-value-in-env :b 2 (set-variable-value-in-env :a 1 (the-empty-environment))))))
  )

(deftest set-variable-value-in-env-two-2-test
  (is (= 2 (lookup-variable-value-in-env 
             :b 
             (set-variable-value-in-env :b 2 (set-variable-value-in-env :a 1 (the-empty-environment))))))
  )

(deftest set-variable-value-in-env-false-test
  (is (= nil (lookup-variable-value-in-env :b (set-variable-value-in-env :a 1 (the-empty-environment)))))
  )

(deftest extend-environment-two-1-test
  (is (= 1 (lookup-variable-value-in-env :a (extend-environment '(:a :b) '(1 2) (the-empty-environment)))))
  )

(deftest extend-environment-two-2-test
  (is (= 2 (lookup-variable-value-in-env :b (extend-environment '(:a :b) '(1 2) (the-empty-environment)))))
  )

(deftest extend-environment-false-test
  (is (= nil (lookup-variable-value-in-env :c (extend-environment '(:a :b) '(1 2) (the-empty-environment)))))
  )

(deftest extend-environment-twice-1-test
  (is (= 3 (lookup-variable-value-in-env 
               :c 
               (extend-environment '(:c :d) '(3 4) (extend-environment '(:a :b) '(1 2) (the-empty-environment))))))
  )

(deftest extend-environment-twice-override-a-test
  (is (= 4 (lookup-variable-value-in-env 
               :a 
               (extend-environment '(:c :a) '(3 4) (extend-environment '(:a :b) '(1 2) (the-empty-environment))))))
  )

(run-tests)
