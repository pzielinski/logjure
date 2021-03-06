;s1 - scheme interpreter that supports general case of self recursion without blowing the stack
;s2 - based on s1, each node is replaced by its children and never visited again
;s3 - based on s2, force-it only done when get variable from env, then update env with forced value
;s4 - based on s3, delay compound proc arg only if arg proc not self-evaluating, and not var-lookup with non delayed value, ARITHMETIC-SUM on INTERVAL shows improvement
;s5 - based on s4, introduced defs, another environment just for definitions, defs are no longer delayed, so proc def closes over correct env;
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
   '/ /
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

(defn make-result
  [env defs returns procs]
    {:env env :defs defs :returns returns :procs procs}
  )

(defn get-result-env
  [result]
    (:env result)
  )

(defn get-result-defs
  [result]
    (:defs result)
  )

(defn get-result-returns
  [result]
    (:returns result)
  )

(defn get-result-procs
  [result]
    (:procs result)
  )

(defn get-result-return
  [result]
    (first (get-result-returns result))
  )

(defn delay-it
  [proc env]
  (list 'thunk proc env)
  )

(defn delay-or-eval
  [proc env defs returns]
  (if (proc env defs nil :delayable?)
    (list 'thunk proc env defs)
    (get-result-return (proc env defs returns :force)))
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

(defn thunk-defs
  [obj]
  (nth obj 3)
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

(defn begin-actions 
  [exp] 
  (rest exp))

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
  (fn [env defs returns mode]
    (if (= mode :delayable?)
      false
      (make-result env defs (cons exp returns) nil)))
  )

(defn eval-variable-lookup-handle-result
  [variable new-result env00 defs00 returns00]
  (let [new-procs (get-result-procs new-result)]
    (if (nil? new-procs)
      ;simple value 
      (let [value (get-result-return new-result)
            env (set-variable-value-in-env variable value env00)
            defs (get-result-defs new-result)
            returns (cons value returns00)]
        (make-result env defs returns nil))
      ;more procs
      (let [new-env (get-result-env new-result)
            new-defs (get-result-defs new-result)
            new-returns (get-result-returns new-result)
            after-proc (fn [env01 defs01  returns01 mode01]
                         (let [value (first returns01)
                               env (set-variable-value-in-env variable value env00)
                               returns (cons value returns00)]
                           (make-result env defs01 returns nil)))
            all-procs (lazy-cat new-procs (list after-proc))]
        (make-result new-env new-defs new-returns all-procs))
      ))
  )

(defn eval-variable-lookup-handle-lookup
  [variable env00 defs00 returns00 mode00]
  (let [value-from-env (lookup-variable-value-in-env variable env00)
        ;get variable value from defs if not found in env
        value (if (nil? value-from-env) (lookup-variable-value-in-env variable defs00) value-from-env)]
    (cond 
      ;delayed (compound proc) arg
      (thunk? value)
      (let [thunk value
            new-proc (thunk-proc thunk)
            new-env (thunk-env thunk)
            new-defs (thunk-defs thunk)
            ;dummy (println " thunk-env# " (hash new-env) "thunk-proc " new-proc)
            new-result (new-proc new-env new-defs returns00 :force)];force-it
        (eval-variable-lookup-handle-result variable new-result env00 defs00 returns00))
      ;regular variable
      :else
      (make-result env00 defs00 (cons value returns00) nil)
      ))  
  )

(defn eval-variable-lookup 
  [variable env00 defs00 returns00 mode00] 
  (cond
    ;check if evaluating looked up variable proc should be delayed
    (= mode00 :delayable?)
    (let [value (lookup-variable-value-in-env variable env00)
          delayed? (thunk? value)]
      delayed?);do not delay if real value is already in env
    ;lookup variable
    :else
    (eval-variable-lookup-handle-lookup variable env00 defs00 returns00 mode00))
  )

;variable lookup
(defn analyze-variable
  [exp] 
  (fn [env00 defs00 returns00 mode00] 
    (eval-variable-lookup exp env00 defs00 returns00 mode00))
  )

(defn analyze-quotation 
  [exp]
  (fn [env defs returns mode]
    (if (= mode :delayable?)
      false
      (make-result env defs (cons (text-of-quotation exp) returns) nil)))
  )

(defn eval-lambda 
  [env defs returns mode params body-proc] 
  (if (= mode :delayable?)
    true
    (make-result env defs (cons (make-procedure params body-proc env) returns) nil))
  )

(defn analyze-lambda 
  [exp] 
  (let [params (lambda-parameters exp)
        body-proc (analyze (lambda-body exp));single expression only, use begin!
        ]
    (fn [env defs returns mode]
      (eval-lambda env defs returns mode params body-proc)))
  )

(defn eval-definition-of-variable 
  [env00 defs00 returns00 mode00 variable value-proc]
  (if (= mode00 :delayable?)
    true
    (make-result
      env00
      defs00
      returns00
      (list
        (fn [env defs returns mode]
          (value-proc env defs returns mode00))
        (fn [env defs returns mode]
          (let [value (first returns)
                new-env (set-variable-value-in-env variable value env00)
                new-defs (set-variable-value-in-env variable value defs)]
            (make-result new-env new-defs returns00 nil)));pass new env
        )))
  )

(defn analyze-definition-of-function 
  [exp] 
  (let [variable (definition-variable exp)
        function-name (first variable)
        params (rest variable)
        body-proc (analyze (definition-value exp))]
    (fn [env defs returns mode]
      (let [value-proc (fn [env defs returns mode] (eval-lambda env defs returns mode params body-proc))]
      (eval-definition-of-variable env defs returns mode function-name value-proc))))
  )

(defn analyze-definition-of-variable 
  [exp] 
  (let [variable (definition-variable exp)
        value-proc (analyze (definition-value exp))]
    (fn [env defs returns mode]
      (eval-definition-of-variable env defs returns mode variable value-proc)))
  )

(defn analyze-definition 
  [exp] 
  (let [variable (definition-variable exp)]
    (if (variable? variable)
      (analyze-definition-of-variable exp)
      (analyze-definition-of-function exp)))
  )

(defn analyze-if 
  [exp]
  (let [predicate-proc (analyze (if-predicate exp))
        consequent-proc (analyze (if-consequent exp))
        alternative-proc (analyze (if-alternative exp))]
    (fn [env00 defs00 returns00 mode00]
      (if (= mode00 :delayable?)
        true
        (make-result
          env00
          defs00
          returns00
          (list
            (fn [env01 defs01 returns mode]
              (predicate-proc env01 defs01 returns :force))
            (fn [env01 defs01 returns mode]
              (let [predicate (first returns)]
                (if (true? predicate)
                  (consequent-proc env01 defs01 returns00 mode00)
                  (alternative-proc env01 defs01 returns00 mode00))
                )))))))
  )

(defn analyze-application
  [exp] 
  (let [operator-proc (analyze (operator exp))
        arg-procs (map #(analyze %) (operands exp))]
    (fn [env00 defs00 returns00 mode00]
      (if (= mode00 :delayable?)
        true
        (make-result
          env00
          defs00
          returns00
          (list 
            (fn [env01 defs01 returns mode]
              (operator-proc env01 defs01 returns :force))
            (fn [env01 defs01 returns mode]
              (let [procedure (first returns)]
                (cond 
                  ;primitive proc
                  (primitive-procedure? procedure)
                  (make-result
                    env01
                    defs01
                    returns00
                    (lazy-cat
                      (map
                        (fn [arg-proc]
                          (fn [env02 defs02 returns mode]
                            (arg-proc env02 defs02 returns :force)))
                        arg-procs)
                      (list
                        (fn [env02 defs02 returns mode]
                          (let [arg-count (count arg-procs)
                                args-reversed (take arg-count returns)
                                args (reverse args-reversed)]
                            (make-result env02 defs02 (cons (apply-primitive-procedure procedure args) returns00) nil)));pass env!!!
                        )))
                  ;compound proc
                  (compound-procedure? procedure)
                  (let [params (procedure-parameters procedure)
                        arg-procs-delayed (map (fn [arg-proc] (delay-or-eval arg-proc env01 defs01 returns)) arg-procs)
                        proc-env (procedure-environment procedure)
                        body-env (extend-environment params arg-procs-delayed proc-env)
                        body-proc (procedure-body procedure)]
                    (make-result
                      env01
                      defs01
                      returns00
                      (list
                        ;run proc body with new env
                        (fn [env02 defs02 returns mode]
                          (body-proc body-env defs02 returns00 mode00));original mode
                        ;restore env and defs
                        (fn [env02 defs02 returns mode]
                          (make-result env01 defs01 returns nil))
                        ))
                    )
                  :else 
                  (error "APPLY - unknown procedure type: " procedure))))
            )))))
  )

(defn analyze-sequence 
  [exps]
  (fn [env00 defs00 returns00 mode00]
    (if (= mode00 :delayable?)
      true
      (make-result
        env00
        returns00
        (map 
          (fn [exp] 
            (let [proc (analyze exp)]
              (fn [env defs returns mode] 
                (proc env defs returns mode)))) 
          exps))))
  )

(defn analyze-begin
  [exp]
  (analyze-sequence (begin-actions exp)) 
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
   'begin analyze-begin
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

(defn analyze-from-map 
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
    (can-analyze-from-map? global-analyze-map exp) (analyze-from-map global-analyze-map exp)
    (define? exp) (analyze-definition exp)
    (application? exp) (analyze-application exp)
    :else (error "Unknown expression type -- EVAL" exp))
  )

(defn debug
  [label env new-env]
  (let [keys-old (keys env)
        keys-new (keys new-env)
        keys-added (apply disj (set keys-new) keys-old)
        keys-removed (apply disj (set keys-old) keys-new)
        keys-carried (apply disj (set keys-new) keys-added)
        keys-changed (filter 
                       (fn[key] 
                         (let[value-old (get env key)
                              value-new (get new-env key)]
                           (not (= value-old value-new)))) 
                       keys-carried)
        added (map 
                  (fn[key]
                    (let[value-new (get new-env key)
                         value-new-real (if (thunk? value-new) (hash value-new) value-new)]
                      {:KEY key :NEW-VAL value-new-real})) 
                  keys-added)
        removed (map 
                  (fn[key]
                    (let[value-old (get env key)
                         value-old-real (if (thunk? value-old) (hash value-old) value-old)]
                      {:KEY key :OLD-VAL value-old-real})) 
                  keys-removed)
        changed (map 
                  (fn[key]
                    (let[value-old (get env key)
                         value-new (get new-env key)
                         value-old-real (if (thunk? value-old) (hash value-old) value-old)
                         value-new-real (if (thunk? value-new) (hash value-new) value-new)]
                      {:KEY key :OLD-VAL value-old-real :NEW-VAL value-new-real})) 
                  keys-changed)
        ]
        (println label 
                 " _OLD=" (hash env) "/" (count env) 
                 " _NEW=" (hash new-env) "/" (count new-env) 
                 " _ADDED=" added 
                 " _REMOVED=" removed 
                 " _CHANGED=" changed)
        )
  )

(defn do-eval 
  ([exp env defs]
    (let [proc (analyze exp)]
      (let [walk 
            (fn walk 
              [procs env defs returns mode]
              (let [proc (first procs)
                    rest-procs (rest procs)
                    result (proc env defs returns mode)
                    new-env (get-result-env result)
                    new-defs (get-result-defs result)
                    ;dbg (debug "ENV* " env new-env)
                    ;dbg (debug "DEF$ " defs new-defs)
                    new-returns (get-result-returns result)
                    new-procs-temp (get-result-procs result)
                    new-procs (lazy-cat new-procs-temp rest-procs)
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
                            new-env (thunk-env thunk)
                            new-defs (thunk-defs thunk)]
                        (recur new-procs new-env new-defs rest-returns mode))));remove trunk from returns
                  (recur new-procs new-env new-defs new-returns mode)
                  )))
            procs (list proc)
            returns '()
            result (walk procs env defs returns :eval)]
        result))
    )
  ([exp env]
    (do-eval exp env (the-empty-environment)))
  )

(defn eval-seq
  ([exps env defs]
    (reduce 
      (fn [result exp] (do-eval exp (get-result-env result) (get-result-defs result)))
      (make-result env defs nil nil) 
      exps))
  ([exps env]
    (eval-seq exps env (the-empty-environment)))
  ([exps]
    (eval-seq exps (setup-environment global-primitive-procedure-impl-map (the-empty-environment)) (the-empty-environment)))
  )


(defn exps-from-str
  [s]
  (let [s1 (clojure.string/replace s #"\r?\n" "")];remove new lines
    (read-string (str \( s1 \)))
    )
  )

(defn exps-from-file
  [filename]
  (let [s (slurp (clojure.java.io/resource filename))]
    (exps-from-str s))
  )

(defn eval-str
  [s]
  (eval-seq (exps-from-str s))
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
            )))))
  )

(comment
  (let [n 10] 
    (println 
      (str "arithmetic-s " n)
      (= 
        (* (/ (+ n 1) 2) n)
        (time
          (get-result-return 
            (eval-seq 
              (list 
                (list 'define 'x n) 
                '(define arithmetic-s (lambda (n sum) (if (= n 0) sum (arithmetic-s (- n 1) (+ n sum))))) 
                '(arithmetic-s x 0)
                )
              (setup-environment global-primitive-procedure-impl-map (the-empty-environment))))))))
)

(comment
  (let [n 10] 
    (println 
      (str "int-arithmetic-s " n)
      (= 
        (* (/ (+ n 1) 2) n)
        (time
          (get-result-return 
            (eval-seq 
              (list 
                (list 'define 'n n) 
                '(define int-arithmetic-s (lambda (start stop sum) (if (> start stop) sum (int-arithmetic-s (+ start 1) stop (+ start sum))))) 
                '(int-arithmetic-s 1 n 0)
                )
              (setup-environment global-primitive-procedure-impl-map (the-empty-environment))))))))
)

(comment
  (let [n 10
        recur-fact
        (fn [n]
          (loop [cnt n acc 1]
            (if (zero? cnt)
              acc
              (recur (dec cnt) (* acc cnt)))))] 
    (println 
      (str "fact " n)
      (= 
        (recur-fact n)
        (time
          (get-result-return 
            (eval-seq 
              (list 
                (list 'define 'n n) 
                '(define fact (lambda (n x) (if (= n 1) x (fact (- n 1) (* n x))))) 
                '(fact n 1)
                )
              (setup-environment global-primitive-procedure-impl-map (the-empty-environment))))))))
)

(comment
  (let [n 10
        recur-fib
        (fn [n]
          (letfn [(fib
                    [current next n]
                    (if (zero? n)
                      current
                      (recur next (+ current next) (dec n))))]
                 (fib 0N 1N n)))] 
    (println 
      (str "fib " n)
      (= 
        (recur-fib n)
        (time
          (get-result-return 
            (eval-seq 
              (list 
                (list 'define 'n n) 
                '(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 
                '(fib n)
                )
              (setup-environment global-primitive-procedure-impl-map (the-empty-environment))))))))
)
