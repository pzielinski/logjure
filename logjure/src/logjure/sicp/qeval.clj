(ns logjure.sicp.qeval
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    logjure.sicp.table
    logjure.sicp.syntax
    logjure.sicp.frame
    logjure.sicp.store
    logjure.sicp.assertion
    logjure.sicp.rule
    )
  )

(declare qeval)

(defn instantiate
  "To instantiate an expression, we copy it, replacing any variables in the expression by their values in a given frame.
The values are themselves instantiated, since they could contain variables (for example, if ?x in exp is bound to ?y
as the result of unification and ?y is in turn bound to 5). The action to take if a variable cannot be instantiated is
given by a procedural argument to instantiate."
  [exp frame unbound-var-handler]
  (let [copy 
        (fn copy [exp]
          (cond (variable? exp)
                (let [the-binding (binding-in-frame exp frame)]
                  (if the-binding
                    (copy (binding-value the-binding))
                    (unbound-var-handler exp frame)))
                (pair? exp)
                  (cons-pair (copy (car exp)) (copy (cdr exp)))
                :else exp))]
        (copy exp))
  )

;SIMPLE
(defn simple-query [query-pattern frame-stream]
  ;(lazy-seq
    (mapcat
      (fn [frame]
        (concat
          (find-assertions query-pattern frame)
          (apply-rules query-pattern frame qeval)))
      frame-stream)
   ; )
  )

;AND
(defn conjoin [conjuncts frame-stream]
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts) frame-stream)))
  )

(put conjoin 'and 'qeval)

;OR
(defn disjoin [disjuncts frame-stream]
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream))))
  )

(put disjoin 'or 'qeval)

;NOT
(defn negate [operands frame-stream]
  (mapcat
    (fn [frame]
      (if (empty? (qeval (negated-query operands) (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream)
  )

(put negate 'not 'qeval)

(defn execute [exp]
  (apply 
    (eval 
      (predicate exp)
      );user-initial-environment) ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (args exp))
  )

;LISP
(defn lisp-value [call frame-stream]
  (mapcat
    (fn [frame]
      (if (execute
            (instantiate
              call
              frame
              (fn [v f]
                (error "Unknown pat var -- LISP-VALUE" v))))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream
    )
  )

(put lisp-value 'lisp-value 'qeval)

(defn always-true [ignore frame-stream] 
  frame-stream
  )

(put always-true 'always-true 'qeval)

(defn qeval [query frame-stream]
  (let [query-type (exp-type query)
        qproc (get-from-table query-type 'qeval)]
    (if qproc
      (qproc (exp-contents query) frame-stream)
      (simple-query query frame-stream)))
  )


