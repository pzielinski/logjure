(ns logjure.utils.pair-test
  (:use 
    logjure.utils.pair
    clojure.test
    )
  )

(deftest test-cons-pair
  (is (= '(:a :b) (cons-pair :a :b)))
  (is (= '(:a ()) (cons-pair :a '())))
  (is (= '(:a nil) (cons-pair :a nil)))
  (is (= '(nil :b) (cons-pair nil :b)))
)

(deftest test-pair?
  (is (= true (pair? (cons-pair :a :b))))
  (is (= true (pair? (cons-pair :a '()))))
  (is (= true (pair? (cons-pair :a nil))))
  (is (= true (pair? (cons-pair nil :b))))
  (is (= true (pair? '())))
  (is (= false (pair? nil)))
  (is (= false (pair? :b)))
)

(deftest test-car
  (is (= :a (car '(:a :b))))
  (is (= nil (car '())))
  (is (= nil (car nil)))
)

(deftest test-cdr
  (is (= :b (cdr '(:a :b))))
  (is (= nil (cdr '())))
  (is (= nil (cdr nil)))
)

(deftest test-set-car!
  (is (= '(:x :b) (set-car! :x '(:a :b))))
  (is (= '(:x nil) (set-car! :x '())))
  (is (= '(:x nil) (set-car! :x nil)))
)

(deftest test-set-cdr!
  (is (= '(:a :x) (set-cdr! :x '(:a :b))))
  (is (= '(nil :x) (set-cdr! :x '())))
  (is (= '(nil :x) (set-cdr! :x nil)))
)

(deftest test-cadr
  (is (= :b (cadr '(:a (:b :c)))))
)

(run-tests)
