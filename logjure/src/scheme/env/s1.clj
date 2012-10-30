;scheme general recursion works, but is very slow, therefore not tested for large recursions 
(ns scheme.env.s1)

(use 'clojure.test)
;(use 'clojure.test.junit)

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

(defn 
  empty-results
  []
   {}
)

(defn 
  set-result
  [proc env result results]
    (assoc results #{proc env} result)
)

(defn 
  del-result
  [proc env results]
    (dissoc results #{proc env})
)

(defn 
  get-result 
  [proc env results]
    (get results #{proc env})
)

(defn make-result
  ([return env]
    {:return return :env env :procs '() :mode :eval}
    )
  ([env procs mode]
    {:return nil :env env :procs procs :mode mode}
    )
  )

(defn make-children
  ([env procs mode]
    {:return nil :env env :procs procs :mode mode}
    )
  )

(defn get-result-return
  [result]
    (:return result)
  )

(defn get-result-env
  [result]
    (:env result)
  )

(defn get-result-procs
  [result]
    (:procs result)
  )

(defn get-result-mode
  [result]
    (:mode result)
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

(defn force-it
  [result results]
  (when (result? result)
    (let [return (get-result-return result)]
      (if (nil? return)
        result
        (if (not (thunk? return))
          result
          (let [thunk return
                new-proc (thunk-proc thunk)
                new-env (thunk-env thunk)
                new-result (new-proc new-env results)]
            (recur new-result results))))))
  )

(defn get-thunk-proc-env-mode-force
  [result results]
  (when (result? result)
    (let [return (get-result-return result)]
      (if (nil? return)
        result
        (if (not (thunk? return))
          result
          (let [thunk return
                proc (thunk-proc thunk)
                env (thunk-env thunk)]
            {:proc proc :env env :mode :force})))))
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
  (fn [env results]
    (make-result exp env)
    )
  )

(defn force-delayed-val 
  [value-delayed-or-not env results] 
  (if (tagged-list? value-delayed-or-not 'delayed-val)
    (let [value-proc (tagged-list-content-1 value-delayed-or-not)
          result (get-result value-proc env results)]
      (if (nil? result)
          (make-children env (list value-proc) :eval)
          result))
    (make-result value-delayed-or-not env))
  )

;variable lookup
(defn analyze-variable
  [exp] 
  (fn [env results] 
    (if (get-result "print-debug" {} results) (println "variable" exp))
    (let [value-delayed-or-not (lookup-variable-value-in-env exp env)
          result-or-children (force-delayed-val value-delayed-or-not env results)]
      result-or-children))
  )

(defn analyze-quotation 
  [exp]
  (fn [env results]
    (make-result (text-of-quotation exp) env)
    )
  )

(defn analyze-definition-of-variable 
  [exp] 
  (let [variable (definition-variable exp)
        value-proc (analyze (definition-value exp))]
    (fn [env results]
        (let [env1 (set-variable-value-in-env variable (list 'delayed-val value-proc) env)
              value-delayed (lookup-variable-value-in-env variable env1)
              result (force-delayed-val value-delayed env1 results)
              value (get-result-return result)]
          (if (nil? value)
            result;children
            (make-result 'ok (set-variable-value-in-env variable value env1));pass new env
            )
          )))
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
    (fn [env results]
      (make-result (make-procedure params body-proc env) env)
      ))
  )

(defn analyze-if 
  [exp]
  (let [predicate-proc (analyze (if-predicate exp))
        consequent-proc (analyze (if-consequent exp))
        alternative-proc (analyze (if-alternative exp))]
    (fn [env results]
      (let [predicate-result (get-result predicate-proc env results)]
        (if (nil? predicate-result)
          (make-children env (list predicate-proc) :force)
          (if (true? (get-result-return predicate-result))
            (let [consequent-result (get-result consequent-proc env results)]
              (if (nil? consequent-result)
                (make-children env (list consequent-proc) :eval)
                consequent-result))
            (let [alternative-result (get-result alternative-proc env results)]
              (if (nil? alternative-result)
                (make-children env (list alternative-proc) :eval)
                alternative-result))
            )))))
  )

(defn analyze-application
  [exp] 
  (let [operator-proc (analyze (operator exp))
        arg-procs (map #(analyze %) (operands exp))]
    (fn [env results] 
      (if (get-result "print-debug" {} results) (println "application" exp))
      (let [procedure-result (get-result operator-proc env results)];force operator!
        (if (nil? procedure-result)
          (make-children env (list operator-proc) :force)
          (let [procedure (get-result-return procedure-result)]
            (cond 
              (primitive-procedure? procedure)
              ;todo: lookup once!
              (let [arg-procs-to-force (filter (fn [arg-proc] (nil? (get-result arg-proc env results))) arg-procs)]
                (if (empty? arg-procs-to-force)
                  (let [args (map (fn [arg-proc] (get-result-return (get-result arg-proc env results))) arg-procs)]
                    ;(println 'apply-primitive-procedure procedure args)
                    (make-result (apply-primitive-procedure procedure args) env))
                  (make-children env arg-procs-to-force :force)))
              (compound-procedure? procedure)
              (let [params (procedure-parameters procedure)
                    arg-procs-delayed (map (fn [arg-proc] (delay-it arg-proc env)) arg-procs)
                    proc-env (procedure-environment procedure)
                    body-env (extend-environment params arg-procs-delayed proc-env)
                    body-proc (procedure-body procedure)]
                ;(println 'apply-compound-procedure 'PROC= procedure 'ARGS= arg-procs-delayed 'END )
                (let [result (get-result body-proc body-env results)]
                  (if (nil? result)
                    (make-children body-env (list body-proc) :eval)
                    result)))
              :else 
              (error "Unknown procedure type -- APPLY" procedure))
            )))))
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
             [items results]
             (let [item (first items)
                   proc (:proc item)
                   env (:env item)
                   mode (:mode item)
                   preliminary-result (proc env results)
                   result (if (= mode :force) 
                            (force-it preliminary-result results) 
                            preliminary-result)
                   new-env (get-result-env result)
                   new-procs (get-result-procs result)
                   new-mode (get-result-mode result)
                   rest-items (rest items)
                   all-items (if (empty? new-procs) 
                               rest-items
                               (let [new-items 
                                     (map 
                                       (fn [proc] {:proc proc :env new-env :mode new-mode}) 
                                       new-procs)]
                                 (concat new-items items)))
                   new-results (if (empty? new-procs) 
                                 (set-result proc env result results) 
                                 results)
                   result-thunk-proc-env (get-thunk-proc-env-mode-force result new-results)
                   final-result? (= result result-thunk-proc-env)
                   final-items (if (and (empty? new-procs) (empty? rest-items))
                                 (list result-thunk-proc-env)
                                 all-items)]
               (if (and (empty? new-procs) (empty? rest-items) final-result?)
                 result
                 (recur final-items new-results))))
           initial-items (list {:proc proc :env env :mode :eval})
           initial-results (set-result "print-debug" {} false (empty-results))
           result (walk initial-items initial-results)]
      (force-it result initial-results)))
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
          (let [output-result (do-eval input env {})
                output-value (get-result-return output-result)
                output-env (get-result-env output-result)]
            (print output-prompt)
            (user-print output-value)
            (recur output-env)
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

;)

(let [recur-fibo 
      (fn [n]
        (letfn [(fib
                  [current next n]
                  (if (zero? n)
                    current
                    (recur next (+ current next) (dec n))))]
               (fib 0N 1N n)))
      env (setup-environment global-primitive-procedure-impl-map (the-empty-environment))
      e1 (get-result-env 
           (do-eval 
             '(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 
             env))]
  (let [n 10
        dummy (println "fib  " n " ")
        expected (time (recur-fibo n))
        e2 (get-result-env (do-eval (list 'define 'n n) e1))
        return (time (get-result-return (do-eval '(fib n) e2)))]
    (println (= expected return)))
  )

)
