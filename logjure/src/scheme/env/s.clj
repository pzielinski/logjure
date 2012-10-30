;trying to speed up
(ns scheme.env.s)

(use 'clojure.test)

(use 'scheme.env.utils)
(use 'scheme.env.syntax)
(use 'scheme.env.env)

;clojure specific
(def apply-in-underlying-interpreter 
  apply)

(def global-primitive-procedure-impl-map
  {
   '* *
   '+ +
   '- -
   '> >
   '< <
   '= =
   '>= >=
   '<= <=
   '== ==
   'car first
   'cdr rest
   'cons cons
   'null? nil?
   }
)

(declare analyze)
(declare do-eval)

(defn result?
  [obj]
    (map? obj)
  )

(defn make-result
  [env returns]
    {:env env :returns returns}
  )

(defn get-result-env
  [result]
    (:env result)
  )

(defn get-result-returns
  [result]
    (:returns result)
  )

(defn get-result-return
  [result]
    (first (get-result-returns result))
  )

(defn delay-it
  [proc env]
  (list 'thunk proc env)
  )

(defn thunk?
  [obj]
  (tagged-list? obj 'thunk)
  )

(defn thunk-proc
  [obj]
  (nth obj 1)
  )

(defn thunk-env
  [obj]
  (nth obj 2)
  )

;todo: when thunk is forced replace it in env!!!
(defn force-it
  [result]
  (if (result? result)
    (let [returns (get-result-returns result)
          return (first returns)
          rest-returns (rest returns)]
      result
      (if (not (thunk? return))
        result
        (let [thunk return
              new-proc (thunk-proc thunk)
              new-env (thunk-env thunk)
              new-result (new-proc new-env rest-returns)];remove trunk from returns
          (recur new-result))))
    result)
  )

(defn primitive-procedure-impl
  [primitive-procedure-impl-map, k]
  (primitive-procedure-impl-map k)
  )

(defn make-if 
  [predicate consequent alternative]
  (list 'if predicate consequent alternative)
  )

(defn make-primitive 
  [implementation] 
  (list 'primitive implementation)
  )

(defn make-primitives-map 
  [primitive-procedure-impl-map]
  (map-the-map identity (fn [x] (make-primitive x)) primitive-procedure-impl-map)
  )

(defn make-procedure 
  [parameters body env] 
  (list 'procedure parameters body env))

(defn apply-primitive-procedure 
  [procedure arguments]
  (apply-in-underlying-interpreter (primitive-implementation procedure) arguments))

(defn definition-variable 
  [exp] 
  (nth exp 1))

(defn definition-value 
  [exp] 
  (nth exp 2))

(defn text-of-quotation 
  [exp] 
  (nth exp 1))

(defn if-alternative 
  [exp] 
  (if (= (count exp) 4) (nth exp 3) 'false))

(defn if-consequent 
  [exp] 
  (nth exp 2))

(defn if-predicate 
  [exp] 
  (nth exp 1))

(defn lambda-body 
  [exp] 
  (nth exp 2))

(defn lambda-parameters 
  [exp] 
  (nth exp 1))

(defn operands 
  [exp] 
  (rest exp))

(defn operator 
  [exp] 
  (first exp))

(defn procedure-body 
  [exp] 
  (nth exp 2))

(defn procedure-environment 
  [exp] 
  (nth exp 3))

(defn procedure-parameters 
  [exp] 
  (nth exp 1))

(defn analyze-self-evaluating 
  [exp] 
  (fn [env returns]
    (make-result env (cons exp returns))
    )
  )

;variable lookup
(defn analyze-variable
  [exp] 
  (fn [env returns] 
    (let [value-delayed-or-not (lookup-variable-value-in-env exp env)]
      (if (tagged-list? value-delayed-or-not 'delayed-val)
        (let [value-proc (tagged-list-content-1 value-delayed-or-not)]
          (list
            (fn [env returns]
              (value-proc env returns)))
          )
        (let [value value-delayed-or-not]
          (make-result env (cons value returns))))))
  )

(defn analyze-quotation 
  [exp]
  (fn [env returns]
    (make-result env (cons (text-of-quotation exp) returns))
    )
  )

(defn analyze-definition-of-variable 
  [exp] 
  (let [variable (definition-variable exp)
        value-proc (analyze (definition-value exp))]
    (fn [env returns00]
        (let [env1 (set-variable-value-in-env variable (list 'delayed-val value-proc) env)]
          (list
            (fn [env returns]
              (value-proc env1 returns)
              )
            (fn [env returns]
              (let [value (first returns)]
                (make-result (set-variable-value-in-env variable value env1) returns00);pass new env
                ))
            ))))
  )

(defn analyze-definition 
  [exp] 
  (let [variable (definition-variable exp)]
    (when (variable? variable)
      (analyze-definition-of-variable exp))
    )
  )

(defn analyze-lambda 
  [exp] 
  (let [params (lambda-parameters exp)
        body-proc (analyze (lambda-body exp));single expression only, use begin!
        ]
    (fn [env returns]
      (make-result env (cons (make-procedure params body-proc env) returns))
      ))
  )

(defn analyze-if 
  [exp]
  (let [predicate-proc (analyze (if-predicate exp))
        consequent-proc (analyze (if-consequent exp))
        alternative-proc (analyze (if-alternative exp))]
    (fn [env00 returns00]
      (list
        (fn [env01 returns]
          (force-it (predicate-proc env01 returns)))
        (fn [env01 returns]
          (let [predicate (first returns)]
            (list
              ;restore env after force-it
              (fn [env02 returns]
                (make-result env00 returns00))
              (if (true? predicate)
                (fn [env02 returns]
                  (consequent-proc env00 returns00))
                (fn [env02 returns]
                  (alternative-proc env00 returns00)))
            )
            )))))
  )

(defn analyze-application
  [exp] 
  (let [operator-proc (analyze (operator exp))
        arg-procs (map #(analyze %) (operands exp))]
    (fn [env00 returns00]
      (list 
        (fn [env01 returns]
          (force-it (operator-proc env01 returns)))
        ;restore env after force-it
        (fn [env01 returns]
          (make-result env00 returns))
        (fn [env01 returns]
          (let [procedure (first returns)]
            (if (primitive-procedure? procedure)
              ;primitive proc
              ;todo: do not return list of lists, flatten it!!!
              (lazy-cat
                (map
                  (fn [arg-proc]
                    (fn [env02 returns]
                      (list
                        (fn [env03 returns]
                          (force-it (arg-proc env03 returns)))
                        ;restore env after force-it
                        (fn [env03 returns]
                          (make-result env02 returns))
                      )))
                  arg-procs)
                (list
                  (fn [env02 returns]
                    (let [arg-count (count arg-procs)
                          args-reversed (take arg-count returns)
                          args (reverse args-reversed)]
                      (make-result env00 (cons (apply-primitive-procedure procedure args) returns00))))
                  ))
              ;compound proc
              (let [params (procedure-parameters procedure)
                    arg-procs-delayed (map (fn [arg-proc] (delay-it arg-proc env01)) arg-procs)
                    proc-env (procedure-environment procedure)
                    body-env (extend-environment params arg-procs-delayed proc-env)
                    body-proc (procedure-body procedure)]
                (list
                  ;set env for proc body
                  (fn [env02 returns]
                    (make-result body-env returns00))
                  ;run proc body with new env
                  (fn [env02 returns]
                    (body-proc env02 returns))
                  ;restore original env
                  (fn [env02 returns]
                    (make-result env00 returns))
                  )
              ))))
      )))
  )

(defn setup-environment
  [primitive-procedure-impl-map env]
  (let [primitives-map (make-primitives-map primitive-procedure-impl-map)
        env1 (set-variable-value-in-env 'true true env)
        env2 (set-variable-value-in-env 'false false env1)]
    (extend-environment-with-map primitives-map env2)
    )
  )

(def global-analyze-map
  {
   'quote analyze-quotation
   'if analyze-if
   'lambda analyze-lambda
   }
)

(defn can-analyze-from-map? 
  [the-map exp]
    (if (empty? exp)
      false
      (not (nil? (the-map (first exp)))))
)

(defn define?
  [exp]
  (if (empty? exp)
    false
    (= 'define (first exp)))
  )

(defn set!?
  [exp]
  (if (empty? exp)
    false
    (= 'set! (first exp)))
  )

(defn do-analyze-from-map 
  [the-map exp]
  (let [proc (the-map (first exp))]
    ;each proc returns fn[env] that returns map
    (proc exp))
)

(defn analyze 
  [exp]
  (cond 
    (self-evaluating? exp) (analyze-self-evaluating exp)
    (variable? exp) (analyze-variable exp)
    (can-analyze-from-map? global-analyze-map exp) (do-analyze-from-map global-analyze-map exp)
    (define? exp) (analyze-definition exp)
    (application? exp) (analyze-application exp)
    :else (error "Unknown expression type -- EVAL" exp))
  )

(defn do-eval 
  [exp env]
  (let [proc (analyze exp)]
    (let [walk 
          (fn walk 
            [procs env returns]
            (let [proc (first procs)
                  rest-procs (rest procs)
                  result (proc env returns)
                  new-env (if (result? result) (get-result-env result) env)
                  new-returns (if (result? result) (get-result-returns result) returns)
                  new-procs (if (result? result) rest-procs (lazy-cat result rest-procs))
                  ]
              (if (empty? new-procs)
                (let [returns (get-result-returns result)
                      return (first returns)
                      rest-returns (rest returns)]
                  (if (not (thunk? return))
                    result
                    (let [thunk return
                          new-proc (thunk-proc thunk)
                          new-procs (list new-proc)
                          new-env (thunk-env thunk)]
                      (recur new-procs new-env rest-returns))));remove trunk from returns
                (recur new-procs new-env new-returns)
                )))
          procs (list proc)
          returns '()
          result (walk procs env returns)]
      result))
  )

(defn user-print 
  [object]
  (if (compound-procedure? object)
    (println 
      (list 
        'compound-procedure
        (procedure-parameters object)
        (procedure-body object)
        '<procedure-env>))
    (println object)
    )
  )

(def input-prompt "in:")

(def output-prompt "=>")

(defn repl
  []
  (loop [env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))]
    (let [input (read)]
      (println input)
      (if (= (str input) "exit")
        (println "exit requested by user")
        (if (nil? input)
          (recur env)
          (let [result (do-eval input env)
                value (get-result-return result)
                env (get-result-env result)]
            (print output-prompt)
            (user-print value)
            (recur env)
            )
          )
        )
      )
    )
  )

(comment
(let [recur-fact
      (fn [n]
        (loop [cnt n acc 1]
          (if (zero? cnt)
            acc
            (recur (dec cnt) (* acc cnt)))))
      env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))
      e1 (get-result-env 
           (do-eval 
             '(define fact (lambda (n x) (if (= n 1) x (fact (- n 1) (* n x))))) 
             env))]
  (let [n 10
        dummy (println "fact " n)
        expected (time (recur-fact n))
        e2 (get-result-env (do-eval (list 'define 'n n) e1))
        return (time (get-result-return (do-eval '(fact n 1) e2)))]
    (println (= expected return)))
  )
)