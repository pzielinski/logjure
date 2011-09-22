(ns logjure.core-sicp)

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

;---------------------------------------------------------------------------------------------------
; NOT IMPLEMENTED

(defn prompt-for-input [input-prompt]
  )

(defn assertion-to-be-added? [exp]
  )

(defn add-assertion-body [exp]
  )

(defn add-rule-or-assertion! [x]
  )

(defn apply-rules [query-pattern frame]
  )

;---------------------------------------------------------------------------------------------------
; CLOJURE INTERFACE

(defn display [& more]
  (println more)
  )

(defn error [& more]
  (display more)
  )

(defn eq? [s1 s2]
  (= s1 s2)
  )

(defn equal? [list1 list2]
  (= list1 list2)
  )

(defn null? [x]
  (nil? x)
  )

(defn string-append [& strs]
  (apply str strs)
  )

(defn string->symbol [str]
  (symbol str)
  )

(defn symbol->string [symbol]
  (str symbol)
  )

(defn number->string [n]
  (str n)
  )

(defn substring [s start end]
  (subs s start end)
  )

(defn string=? [str1 str2]
  (= str1 str2)
  )

(defn string-length [str]
  (.length str)
  )

;---------------------------------------------------------------------------------------------------
; PAIR

(defn cons-pair [a b]
  (list a b)
  )

(defn car [pair]
  (first pair)
  )

(defn cdr [pair]
  (second pair)
  )

(defn caar [pair]
  (car (car pair))
  )

(defn cadr [pair]
  (car (cdr pair))
  )

(defn caddr [pair]
  (car (cdr (cdr pair)))
  )

(defn cddr [pair]
  (cdr (cdr pair))
  )

(defn pair? [exp]
  (list? exp)
  )

(defn set-car! [x pair]
  (cons-pair x (cdr pair))
  );ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn set-cdr! [x pair]
  (cons-pair (car pair) x)
  );ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn tagged-list? [exp tag]
  (if (pair? exp)
    (eq? (car exp) tag)
    false)
  )

;---------------------------------------------------------------------------------------------------
; MAP/TABLE EMULATION WITH PAIR

(defn make-table []
  (list '*table*)
  )

(defn assoc- [key records]
  (cond (null? records) 
        false
        (equal? key (caar records)) 
        (car records)
        :else 
        (assoc- key (cdr records)));NEED RECUR !!!!!!!!!!!!!
  )

(defn lookup [key table]
  (let [record (assoc- key (cdr table))]
    (if record
      (cdr record)
      false))
  )

(defn lookup [key-1 key-2 table]
  (let [subtable (assoc- key-1 (cdr table))]
    (if subtable
      (let [record (assoc- key-2 (cdr subtable))]
        (if record
          (cdr record)
          false))
      false))
  )

(defn insert! [key value table]
  (let [record (assoc- key (cdr table))]
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons-pair (cons-pair key value) (cdr table)))))
  'ok
  );table has to be an ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn insert! [key-1 key-2 value table]
  (let [subtable (assoc- key-1 (cdr table))]
    (if subtable
      (let [record (assoc- key-2 (cdr subtable))]
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons-pair (cons-pair key-2 value) (cdr subtable)))))
      (set-cdr! table
                (cons-pair 
                  (list key-1 (cons-pair key-2 value));USES LIST explicitely !!!!!!!!!!!!!!!!!!!!!!!
                  (cdr table)))))
  'ok
  );table has to be an ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn make-table-local []
  (let [local-table (make-table)]
    (fn dispatch [m]
      (cond (eq? m 'lookup-proc) 
            (fn [key-1 key-2] (lookup key-1 key-2 local-table))
            (eq? m 'insert-proc!)
            (fn [key-1 key-2 value] (insert! key-1 key-2 value local-table))
            :else 
            (error "Unknown operation -- TABLE" m))))
  )

(defn operation-table [dispatch-value]
  ((make-table-local) dispatch-value)
  )

(defn get-from-table [key-1 key-2] 
  ((operation-table 'lookup-proc) key-1 key-2)
  )

;this will not work! operation-table will create new table!!!!!!!!!!!!!!
(defn put [key-1 key-2 value] 
  ((operation-table 'insert-proc!) key-1 key-2 value) 
  )

;---------------------------------------------------------------------------------------------------
; STREAM

(def the-empty-stream '())

(defn stream-null? [stream]
  (null? stream)
  )

(defn cons-stream [x s]
    (cons-pair x (delay s))
  )

(defn stream-car [stream] 
  (car stream)
  )

(defn stream-cdr [stream] 
  (force (cdr stream))
  )

(defn singleton-stream [x]
  (cons-stream x the-empty-stream)
  )

(defn stream-append-delayed [s1 delayed-s2]
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (stream-append-delayed (stream-cdr s1) delayed-s2)));NEED RECUR !!!!!!!!!!!!!
  )

(defn interleave-delayed [s1 delayed-s2]
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (interleave-delayed (force delayed-s2) (delay (stream-cdr s1)))));NEED RECUR !!!!!!!!!!!!!
  )

(defn flatten-stream [stream]
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream)))));NEED RECUR !!!!!!!!!!!!!
  )

(defn stream-map [proc stream]
  (if (stream-null? stream)
    the-empty-stream
    (cons-stream 
      (proc (stream-car stream)) 
      (stream-map proc (stream-cdr stream))));NEED RECUR !!!!!!!!!!!!!
  )

(defn stream-flatmap [proc stream]
  (flatten-stream (stream-map proc stream))
  )

(defn display-stream [stream]
  (stream-map display stream)
  )

;---------------------------------------------------------------------------------------------------
; SYNTAX

(defn variable? [exp]
  (tagged-list? exp '?)
  )

(defn constant-symbol? [exp] 
  (symbol? exp)
  )

(defn empty-conjunction? [exps] 
  (null? exps)
  )

(defn first-conjunct [exps] 
  (car exps)
  )

(defn rest-conjuncts [exps] 
  (cdr exps)
  )

(defn empty-disjunction? [exps] 
  (null? exps)
  )

(defn first-disjunct [exps] 
  (car exps)
  )

(defn rest-disjuncts [exps] 
  (cdr exps)
  )

(defn negated-query [exps] 
  (car exps)
  )

(defn predicate [exps] 
  (car exps)
  )

(defn args [exps] 
  (cdr exps)
  )

(defn rule? [statement]
  (tagged-list? statement 'rule)
  )

(defn conclusion [rule] 
  (cadr rule)
  )

(defn rule-body [rule]
  (if (null? (cddr rule))
    '(always-true)
    (caddr rule))
  )

(defn expand-question-mark [symbol]
  (let [chars (symbol->string symbol)]
    (if (string=? (substring chars 0 1) "?")
      (cons-pair '? (string->symbol (substring chars 1 (string-length chars))))
      symbol))
  )

(defn map-over-symbols [proc exp]
  (cond (pair? exp) 
        (cons-pair 
          (map-over-symbols proc (car exp));NEED RECUR !!!!!!!!!!!!!
          (map-over-symbols proc (cdr exp)));NEED RECUR !!!!!!!!!!!!!
        (constant-symbol? exp)
        (proc exp)
        :else 
        exp
        )
  )

(defn query-syntax-process [exp]
  (map-over-symbols expand-question-mark exp)
  )

(def *rule-counter* (atom 0))

(defn new-rule-application-id []
  (swap! *rule-counter* inc)
  )

(defn make-new-variable [variable rule-application-id]
  (cons-pair '? (cons-pair rule-application-id (cdr variable)))
  )

(defn contract-question-mark [variable]
  (string->symbol
    (string-append "?"
                   (if (pair? (cdr variable));FIXED BUG !!! was: (number? (cadr variable))
                     (string-append (symbol->string (cddr variable));FIXED BUG !!! was caddr
                                    "-"
                                    (number->string (cadr variable)))
                     (symbol->string (cdr variable)))));FIXED BUG !!! was: (cadr variable)
  )

;---------------------------------------------------------------------------------------------------
; FRAME

(defn make-empty-frame []
  '()
  )

(defn make-binding [variable value]
  (cons-pair variable value)
  )

(defn binding-variable [binding]
  (car binding)
  )

(defn binding-value [binding]
  (cdr binding)
  )

(defn binding-in-frame [variable frame]
  (assoc- variable frame)
  )

(defn extend-frame [variable value frame]
  (cons-pair (make-binding variable value) frame))

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

;---------------------------------------------------------------------------------------------------
; ASSERTIONS

(def THE-ASSERTIONS the-empty-stream)

(defn get-all-assertions [] 
  THE-ASSERTIONS
  )

(defn use-index? [pat]
  (constant-symbol? (car pat))
  )

(defn index-key-of [pat]
  (let [key (car pat)]
    (if (variable? key) 
      '? 
      key))
  )

(defn get-stream [key1 key2]
  (let [s (get-from-table key1 key2)];get-from-table needs a table !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (if s s 
      the-empty-stream))
  )

(defn get-indexed-assertions [pattern]
  (get-stream (index-key-of pattern) 'assertion-stream)
  )

(defn fetch-assertions [pattern frame]
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions))
  )

;---------------------------------------------------------------------------------------------------
; SIMPLE QUERY

(declare extend-if-consistent qeval)

(defn pattern-match
  "The basic pattern matcher returns either the symbol failed or an extension of the given frame. The basic idea of
the matcher is to check the pattern against the data, element by element, accumulating bindings for the pattern
variables. If the pattern and the data object are the same, the match succeeds and we return the frame of bindings
accumulated so far. Otherwise, if the pattern is a variable we extend the current frame by binding the variable to the
data, so long as this is consistent with the bindings already in the frame. If the pattern and the data are both pairs,
we (recursively) match the car of the pattern against the car of the data to produce a frame; in this frame we then
match the cdr of the pattern against the cdr of the data. If none of these cases are applicable, the match fails and we
return the symbol failed."
  [pat dat frame]
  (cond (eq? frame 'failed) 'failed
        (equal? pat dat) frame
        (variable? pat) (extend-if-consistent pat dat frame)
        (and (pair? pat) (pair? dat)) (pattern-match (cdr pat)
                                                     (cdr dat)
                                                     (pattern-match (car pat);NEED RECUR !!!!!!!!!!!!!
                                                                    (car dat)
                                                                    frame))
        :else 'failed)
  )

(defn extend-if-consistent
  "Here is the procedure that extends a frame by adding a new binding, if this is consistent with the bindings already in
the frame.
If there is no binding for the variable in the frame, we simply add the binding of the variable to the data. Otherwise
we match, in the frame, the data against the value of the variable in the frame. If the stored value contains only
constants, as it must if it was stored during pattern matching by extend-if-consistent, then the match simply
tests whether the stored and new values are the same. If so, it returns the unmodified frame; if not, it returns a
failure indication. The stored value may, however, contain pattern variables if it was stored during unification (see
section 4.4.4.4). The recursive match of the stored pattern against the new data will add or check bindings for the
variables in this pattern. For example, suppose we have a frame in which ?x is bound to (f ?y) and ?y is unbound,
and we wish to augment this frame by a binding of ?x to (f b). We look up ?x and find that it is bound to (f ?y).
This leads us to match (f ?y) against the proposed new value (f b) in the same frame. Eventually this match
extends the frame by adding a binding of ?y to b. ?X remains bound to (f ?y). We never modify a stored binding
and we never store more than one binding for a given variable."
  [variable dat frame]
  (let [binding (binding-in-frame variable frame)]
    (if binding
      (pattern-match (binding-value binding) dat frame) ;TRAMPOLINE or move to pattern-match!!!!!!!!!!!!!!
      (extend-frame variable dat frame)))
  )

(defn check-an-assertion
  "Check-an-assertion takes as arguments a pattern, a data object (assertion), and a frame and returns either a oneelement
stream containing the extended frame or the-empty-stream if the match fails."
  [assertion query-pat query-frame]
  (let [match-result (pattern-match query-pat assertion query-frame)]
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result)))
  )

(defn find-assertions
  "Find-assertions, called by simple-query (section 4.4.4.2), takes as input a pattern and a frame. It returns a
stream of frames, each extending the given one by a data-base match of the given pattern. It uses fetchassertions
(section 4.4.4.5) to get a stream of all the assertions in the data base that should be checked for a match
against the pattern and the frame. The reason for fetch-assertions here is that we can often apply simple tests
that will eliminate many of the entries in the data base from the pool of candidates for a successful match. The
system would still work if we eliminated fetch-assertions and simply checked a stream of all assertions in the
data base, but the computation would be less efficient because we would need to make many more calls to the
matcher."
  [pattern frame]
  (stream-flatmap 
    (fn [datum] (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame))
  )

(defn simple-query [query-pattern frame-stream]
  (stream-flatmap
    (fn [frame]
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream)
  )

;---------------------------------------------------------------------------------------------------
; COMPOUNT QUERY

;AND
(defn conjoin [conjuncts frame-stream]
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts) frame-stream)))
  )

(put 'and 'qeval conjoin)

(defn disjoin [disjuncts frame-stream]
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream))))
  )

(put 'or 'qeval disjoin)

(defn negate [operands frame-stream]
  (stream-flatmap
    (fn [frame]
      (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream)
  )

(put 'not 'qeval negate)

(defn execute [exp]
  (apply 
    (eval 
      (predicate exp)
      );user-initial-environment) 
    (args exp))
  )

(defn lisp-value [call frame-stream]
  (stream-flatmap
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

(put 'lisp-value 'qeval lisp-value)

(defn always-true [ignore frame-stream] 
  frame-stream
  )

(put 'always-true 'qeval always-true)

;---------------------------------------------------------------------------------------------------
; REST

(defn get-type [query]
  (car query)
  )

(defn get-contents [query]
  (cdr query)
  )

(defn get-procedure [query-type procedure-name]
  )

(defn qeval [query frame-stream]
  (let [query-type (get-type query)
        qproc (get-procedure query-type 'qeval)]
    (if qproc
      (qproc (get-contents query) frame-stream)
      (simple-query query frame-stream)))
  )

(defn read-input []
  ;(read)
  ;(read-line)
  )

(defn query-driver-loop []
  (prompt-for-input input-prompt)
  (let [q (query-syntax-process (read-input))]
    (cond (assertion-to-be-added? q)
          (do
            (add-rule-or-assertion! (add-assertion-body q))
            (newline)
            (display "Assertion added to data base.")
            (query-driver-loop))
          :else
          (do
            (newline)
            (display output-prompt)
            (display-stream
              (stream-map
                (fn [frame]
                        (instantiate q
                                     frame
                                     (fn [v f]
                                             (contract-question-mark v))))
                (qeval q (singleton-stream (make-empty-frame)))))
            (query-driver-loop))
          )))

