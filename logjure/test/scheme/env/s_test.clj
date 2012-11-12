(ns scheme.env.s-test)

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

(deftest analyze-from-map-test 
  (is (= 'x (get-result-return ((analyze-from-map global-analyze-map '(quote x)) {} '() :eval))))
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
    ;self recursion
    ;ARITHMETIC-SUM
    (let [e1 (get-result-env 
               (do-eval 
                 '(define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                 env))]
      (is (= 1 (get-result-return (do-eval '(arithmetic-s 1 0) e1))))
      (is (= 3 (get-result-return (do-eval '(arithmetic-s 2 0) e1))))
      (is (= 6 (get-result-return (do-eval '(arithmetic-s 3 0) e1))))
      (is (= 10 (get-result-return (do-eval '(arithmetic-s 4 0) e1))))
      (let [n 10
            sum (* (/ (+ n 1) 2) n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))]
        (is (= sum (get-result-return (do-eval '(arithmetic-s n 0) e2)))))
      )
    ;ARITHMETIC-SUM on INTERVAL
    (let [e1 (get-result-env 
               (do-eval 
                 '(define int-arithmetic-s (lambda (start stop sum) (if (> start stop) sum (int-arithmetic-s (+ start 1) stop (+ start sum))))) 
                 env))]
      (is (= 1 (get-result-return (do-eval '(int-arithmetic-s 1 1 0) e1))))
      (is (= 3 (get-result-return (do-eval '(int-arithmetic-s 1 2 0) e1))))
      (is (= 6 (get-result-return (do-eval '(int-arithmetic-s 1 3 0) e1))))
      (is (= 10 (get-result-return (do-eval '(int-arithmetic-s 1 4 0) e1))))
      (let [n 10
            sum (* (/ (+ n 1) 2) n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))]
        (is (= sum (get-result-return (do-eval '(int-arithmetic-s 1 n 0) e2)))))
      )
    ;FACTORIAL
    (let [recur-fact
          (fn [n]
            (loop [cnt n acc 1]
              (if (zero? cnt)
                acc
                (recur (dec cnt) (* acc cnt)))))
          ;env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))
          e1 (get-result-env 
               (do-eval 
                 '(define fact (lambda (n x) (if (= n 1) x (fact (- n 1) (* n x))))) 
                 env))]
      (let [n 10
            ;dummy (println "fact " n)
            expected (recur-fact n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))
            return (get-result-return (do-eval '(fact n 1) e2))]
        (is (= expected return)))
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
          ;env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))
          e1 (get-result-env 
               (do-eval 
                 '(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 
                 env))]
      (let [n 10
            ;dummy (println "fib " n)
            expected (recur-fibo n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))
            return (get-result-return (do-eval '(fib n) e2))]
        (is (= expected return)))
      )
    ;mutual recursion
    ;EVEN-ODD
    (let [;env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))
          e0 (get-result-env 
               (do-eval 
                 '(define odd? (lambda (n) (if (= n 0) false (even? (- n 1))))) 
                 env))
          e1 (get-result-env 
               (do-eval 
                 '(define even? (lambda (n) (if (= n 0) true (odd? (- n 1))))) 
                 e0))]
      (let [n 10
            ;dummy (println "even-odd " n)
            expected (odd? n)
            e2 (get-result-env (do-eval (list 'define 'n n) e1))
            return (get-result-return (do-eval '(odd? n) e2))]
        (is (= expected return)))
      )
    )
  )

(deftest eval-seq-test 
  (let [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    ;primitive
    (is (= false (get-result-return (eval-seq (list 'false) env))))
    ;self-evaluating
    (is (= 1 (get-result-return (eval-seq (list 1) env))))
    ;variable
    (is (= 1 (get-result-return (eval-seq (list 'v) (extend-environment-with-map {'v 1} env)))))
    ;definition
    (is (= 2 (get-result-return (eval-seq (list '(define x 2) 'x) env))))
    (is (= 3 (get-result-return (eval-seq (list '(define x 2) '(define y (+ 1 x)) 'y) env))))
    (is (= 5 (get-result-return (eval-seq (list '(define x 2) '(define y (+ 1 x)) '(define z (+ x y)) 'z) env))))
    ;aplication
    (is (= 6 (get-result-return (eval-seq (list '(define doubler (lambda (x) (+ x x))) '(doubler 3)) env))))
    ;self recursion
    (let [n 10] 
      (is (= (* (/ (+ n 1) 2) n) 
             (get-result-return 
               (eval-seq 
                 (list 
                   (list 'define 'n n) 
                   '(define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                   '(arithmetic-s n 0)
                   )
                 env)))))
    ;mutual recursion
    (let [n 10] 
      (is (= (odd? n) 
             (get-result-return 
               (eval-seq 
                 (list 
                   (list 'define 'n n) 
                   '(define odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
                   '(define even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                   '(odd? n)
                   ) 
                 env)))))
    )
  )

(deftest eval-seq-exps-from-str-test 
  (let [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    ;primitive
    (is (= false (get-result-return (eval-seq (exps-from-str "false") env))))
    ;self-evaluating
    (is (= 1 (get-result-return (eval-seq (exps-from-str "1") env))))
    ;variable
    (is (= 1 (get-result-return (eval-seq (exps-from-str "v") (extend-environment-with-map {'v 1} env)))))
    ;definition
    (is (= 2 (get-result-return (eval-seq (exps-from-str "(define x 2) x") env))))
    (is (= 3 (get-result-return (eval-seq (exps-from-str "(define x 2) (define y (+ 1 x)) y") env))))
    (is (= 5 (get-result-return (eval-seq (exps-from-str "(define x 2) (define y (+ 1 x)) (define z (+ x y)) z") env))))
    ;aplication
    (is (= 6 (get-result-return (eval-seq (exps-from-str "(define doubler (lambda (x) (+ x x))) (doubler 3)") env))))
    ;self recursion
    (let [n 10] 
      (is (= (* (/ (+ n 1) 2) n) 
             (get-result-return 
               (eval-seq 
                 (exps-from-str (str " 
                   (define n "n") 
                   (define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                   (arithmetic-s n 0)"
                   ))
                 env)))))
    ;mutual recursion
    (let [n 10] 
      (is (= (odd? n) 
             (get-result-return 
               (eval-seq 
                 (exps-from-str (str "
                   (define n "n") 
                   (define odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
                   (define even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                   (odd? n)"
                   )) 
                 env)))))
    )
  )

(deftest eval-seq-exps-from-file-test 
  (let [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    ;primitive application
    (is (= 3 (get-result-return (eval-seq (exps-from-file "scheme/env/test01.scj") env))))
    )
  )

(run-tests)
