(ns scheme.interpreter.environmental.s-test)

(use 'clojure.test)
;(use 'clojure.test.junit)

(use 'scheme.env.env)
(use 'scheme.env.s)

(deftest primitive-procedure-impl-test
  (is (= first (primitive-procedure-impl (hash-map 'car first) 'car)))
  )

(deftest make-primitive-test 
  (is (= (list 'primitive cons) (make-primitive cons)))
  )

(deftest can-analyze-from-map?-test 
  (is (= true (can-analyze-from-map? global-analyze-map '(quote x))))
  (is (= false (can-analyze-from-map? global-analyze-map '(x))))
  (is (= false (can-analyze-from-map? global-analyze-map '())))
  )

(deftest do-analyze-from-map-test 
  (is (= 'x (get-result-return ((do-analyze-from-map global-analyze-map '(quote x)) nil {}))))
  )

(deftest do-eval-test 
  (let [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    ;primitive
    (is (= false (get-result-return (do-eval 'false env))))
    (is (= true (get-result-return (do-eval 'true env))))
    (is (= (make-primitive +) (get-result-return (do-eval '+ env))))
    (is (= (make-primitive first) (get-result-return (do-eval 'car env))))
    ;self-evaluating
    (is (= 1 (get-result-return (do-eval 1 env))))
    (is (= "" (get-result-return (do-eval "" env))))
    (is (= "1" (get-result-return (do-eval "1" env))))
    ;variable
    ;(is (thrown? Throwable (do-eval 'v env)) "Variable is not bound!");!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (is (= 1 (get-result-return (do-eval 'v (extend-environment-with-map {'v 1} env)))))
    ;quoted
    (is (= 'x (get-result-return (do-eval '(quote x) env))))
    ;definition
    (let [e1 (extend-environment-with-map '{} env)]
      (let [e2 (get-result-env (do-eval '(define x 2) e1))]
        (is (= 2 (get-result-return (do-eval 'x e2))))
        (let [e3 (get-result-env (do-eval '(define y (+ 1 x)) e2))]
          (is (= 3 (get-result-return (do-eval 'y e3))))
          (let [e4 (get-result-env (do-eval '(define z (+ x y)) e3))];same here, x causes stack overflow!
            (is (= 5 (get-result-return (do-eval 'z e4))))
            (let [e5 (get-result-env (do-eval '(define doubler (lambda (x) (+ x x))) e4))]
              (is (= 6 (get-result-return (do-eval '(doubler 3) e5))))
              )))))
    ;if
    (is (= 1 (get-result-return (do-eval '(if true 1 2) env))))
    (is (= 2 (get-result-return (do-eval '(if false 1 2) env))))
    (is (= 'a (get-result-return (do-eval '(if (> 2 1) 'a 'b) env))))
    (is (= 3 (get-result-return (do-eval '(if (> 2 1) (+ 1 2) 'b) env))))
    (is (= 5 (get-result-return (do-eval '(if (< 2 1) (+ 1 2) (- 7 2)) env))))
    ;lambda
    (is (= 5 (get-result-return (do-eval '((lambda (a b) (+ a b)) 2 3) env))))
    ;clojure
    (let [e1 (get-result-env (do-eval '(define adder (lambda (a) (lambda (x) (+ x a)))) env))
          e2 (get-result-env (do-eval '(define add2 (adder 2)) e1))]
      (is (= 5 (get-result-return (do-eval '(add2 3) e2))))
      (is (= 7 (get-result-return (do-eval '((adder 3) 4) e1))))
      )
    ;application
    (is (= 2 (get-result-return (do-eval '(* 1 2) env))))
    (is (= 3 (get-result-return (do-eval '((lambda (a b) (+ a b)) 1 2) env))))
    (is (= 5 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) 2) env))))
    (is (= 10 (get-result-return (do-eval '((lambda (a b) (+ a b)) (+ 1 2) ((lambda (a b) (+ a b)) 3 4)) env))))
    ;recursion
    ;FACTORIAL
    (let [factorial
          (fn [n]
            (loop [cnt n acc 1]
              (if (zero? cnt)
                acc
                (recur (dec cnt) (* acc cnt)))))
          e1 (get-result-env (do-eval '(define fact (lambda (n x) (if (= n 1) x (fact (- n 1) (* n x))))) env))]
      (is (= 1 (get-result-return (do-eval '(fact 1 1) e1))))
      (is (= 2 (get-result-return (do-eval '(fact 2 1) e1))))
      (is (= 6 (get-result-return (do-eval '(fact 3 1) e1))))
      (is (= 24 (get-result-return (do-eval '(fact 4 1) e1))))
      )
    ;ARITHMETIC-SUM
    (let [e1 (get-result-env 
               (do-eval 
                 '(define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                 env))]
      (is (= 1 (get-result-return (do-eval '(arithmetic-s 1 0) e1))))
      (is (= 3 (get-result-return (do-eval '(arithmetic-s 2 0) e1))))
      (is (= 6 (get-result-return (do-eval '(arithmetic-s 3 0) e1))))
      (is (= 10 (get-result-return (do-eval '(arithmetic-s 4 0) e1))))
      (let [n 5
            sum (* (/ (+ n 1) 2) n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))]
        (is (= sum (get-result-return (do-eval '(arithmetic-s n 0) e2)))))
      )
    ;FIBONACCI
    (let [recur-fibo 
          (fn [n]
            (letfn [(fib
                      [current next n]
                      (if (zero? n)
                        current
                        (recur next (+ current next) (dec n))))]
                   (fib 0N 1N n)))
          e1 (get-result-env 
               (do-eval 
                 '(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 
                 env))]
      (is (= (recur-fibo 1) (get-result-return (do-eval '(fib 1) e1))))
      (let [n 10
            expected (recur-fibo n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))]
        (is (= expected (get-result-return (do-eval '(fib n) e2)))))
      )
    )
  )

(run-tests)
