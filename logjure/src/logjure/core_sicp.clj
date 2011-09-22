(ns logjure.core-sicp)

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

;---------------------------------------------------------------------------------------------------
; NOT IMPLEMENTED

(defn prompt-for-input [input-prompt]
  (println input-prompt)
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

(defn stream-append [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (stream-append (stream-cdr s1) s2)));NEED RECUR !!!!!!!!!!!!!
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

(defn exp-type [exp]
  (if (pair? exp)
    (car exp)
    (error "Unknown expression TYPE" exp))
  )

(defn exp-contents [exp]
  (if (pair? exp)
    (cdr exp)
    (error "Unknown expression CONTENTS" exp))
  )

(defn variable? [exp]
  (tagged-list? exp '?)
  )

(defn constant-symbol? [exp] 
  (symbol? exp)
  )

(defn assertion-to-be-added? [exp]
  (eq? (exp-type exp) 'assert!)
  )

(defn add-assertion-body [exp]
  (car (exp-contents exp))
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
; ASSERTIONS & RULES DATABASE

(def THE-ASSERTIONS the-empty-stream);SHOULD BE AN ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn get-all-assertions [] 
  THE-ASSERTIONS
  )

(defn indexable? [pat]
  (or (constant-symbol? (car pat)) (variable? (car pat)))
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

(defn fetch-assertions
  "One important problem in designing logic programming languages is that of arranging things so that as few
irrelevant data-base entries as possible will be examined in checking a given pattern. In our system, in addition to
storing all assertions in one big stream, we store all assertions whose cars are constant symbols in separate streams,
in a table indexed by the symbol. To fetch an assertion that may match a pattern, we first check to see if the car of
the pattern is a constant symbol. If so, we return (to be tested using the matcher) all the stored assertions that have
the same car. If the pattern's car is not a constant symbol, we return all the stored assertions. Cleverer methods
could also take advantage of information in the frame, or try also to optimize the case where the car of the pattern
is not a constant symbol. We avoid building our criteria for indexing (using the car, handling only the case of
constant symbols) into the program; instead we call on predicates and selectors that embody our criteria."
  [pattern frame]
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions))
  )

(def THE-RULES the-empty-stream);SHOULD BE AN ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn get-all-rules [] 
  THE-RULES
  )

(defn get-indexed-rules [pattern]
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream))
  )

(defn fetch-rules
  "Rules are stored similarly, using the car of the rule conclusion. Rule conclusions are arbitrary patterns, however, so
they differ from assertions in that they can contain variables. A pattern whose car is a constant symbol can match
rules whose conclusions start with a variable as well as rules whose conclusions have the same car. Thus, when
fetching rules that might match a pattern whose car is a constant symbol we fetch all rules whose conclusions start
with a variable as well as those whose conclusions have the same car as the pattern. For this purpose we store all
rules whose conclusions start with a variable in a separate stream in our table, indexed by the symbol ?."
  [pattern frame]
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules))
  )

(defn store-assertion-in-index [assertion]
  (if (indexable? assertion)
    (let [key (index-key-of assertion)]
      (let [current-assertion-stream (get-stream key 'assertion-stream)]
        (put 
          key 
          'assertion-stream
          (cons-stream assertion current-assertion-stream)))))
  )

(defn store-rule-in-index [rule]
  (let [pattern (conclusion rule)]
    (if (indexable? pattern)
      (let [key (index-key-of pattern)]
        (let [current-rule-stream (get-stream key 'rule-stream)]
          (put 
            key
            'rule-stream
            (cons-stream rule current-rule-stream))))))
  )

(defn add-assertion! [assertion]
  (store-assertion-in-index assertion)
  (let [old-assertions THE-ASSERTIONS]
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions));ATOM!!!!!!!!!!!!
    'ok)
  )

(defn add-rule! [rule]
  (store-rule-in-index rule)
  (let [old-rules THE-RULES]
    (set! THE-RULES (cons-stream rule old-rules));ATOM!!!!!!!!!!!!
    'ok)
  )

(defn add-rule-or-assertion! [assertion]
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion))
  )

;---------------------------------------------------------------------------------------------------
; MATCH ASSERTIONS

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

;---------------------------------------------------------------------------------------------------
; COMPOUND QUERY

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
      );user-initial-environment) ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
; MATCH RULES

(defn depends-on?
  "Depends-on? is a predicate that tests whether an expression proposed to be the value of a pattern variable depends
on the variable. This must be done relative to the current frame because the expression may contain occurrences of
a variable that already has a value that depends on our test variable. The structure of depends-on? is a simple
recursive tree walk in which we substitute for the values of variables whenever necessary."
  [exp var frame]
  (letfn [(tree-walk 
          [e]
          (cond 
            (variable? e)
            (if (equal? var e)
              true
              (let [b (binding-in-frame e frame)]
                (if b
                  (tree-walk (binding-value b))
                  false)))
            (pair? e)
            (or (tree-walk (car e)) (tree-walk (cdr e)))
            :else false))]
         (tree-walk exp))
  )

(declare unify-match)

(defn extend-if-possible
  "In unification, as in one-sided pattern matching, we want to accept a proposed extension of the frame only if it is
consistent with existing bindings. The procedure extend-if-possible used in unification is the same as the
extend-if-consistent used in pattern matching except for two special checks, marked ``***'' in the program
below. In the first case, if the variable we are trying to match is not bound, but the value we are trying to match it
with is itself a (different) variable, it is necessary to check to see if the value is bound, and if so, to match its value.
If both parties to the match are unbound, we may bind either to the other.
The second check deals with attempts to bind a variable to a pattern that includes that variable. Such a situation can
occur whenever a variable is repeated in both patterns. Consider, for example, unifying the two patterns (?x ?x)
and (?y <expression involving ?y>) in a frame where both ?x and ?y are unbound. First ?x is matched against
?y, making a binding of ?x to ?y. Next, the same ?x is matched against the given expression involving ?y. Since ?x
is already bound to ?y, this results in matching ?y against the expression. If we think of the unifier as finding a set
of values for the pattern variables that make the patterns the same, then these patterns imply instructions to find a ?
y such that ?y is equal to the expression involving ?y. There is no general method for solving such equations, so we
reject such bindings; these cases are recognized by the predicate depends-on?.80 On the other hand, we do not want
to reject attempts to bind a variable to itself. For example, consider unifying (?x ?x) and (?y ?y). The second
attempt to bind ?x to ?y matches ?y (the stored value of ?x) against ?y (the new value of ?x). This is taken care of
by the equal? clause of unify-match."
  [var val frame]
  (let [binding (binding-in-frame var frame)]
    (cond 
      binding
      (unify-match (binding-value binding) val frame);TRAMPOLINE/RECUR!!!!!!!!!!!!!!!
      (variable? val) ; ***
      (let [binding (binding-in-frame val frame)]
        (if binding
          (unify-match var (binding-value binding) frame);TRAMPOLINE/RECUR!!!!!!!!!!!!!!!
          (extend-frame var val frame)))
      (depends-on? val var frame) ; ***
      'failed
      :else (extend-frame var val frame)))
  )

(defn unify-match
  "The unification algorithm is implemented as a procedure that takes as inputs two patterns and a frame and returns
either the extended frame or the symbol failed. The unifier is like the pattern matcher except that it is symmetrical
-- variables are allowed on both sides of the match. Unify-match is basically the same as pattern-match, except
that there is extra code (marked ``***'' below) to handle the case where the object on the right side of the match is a
variable."
  [p1 p2 frame]
  (cond 
    (eq? frame 'failed) 
    'failed
    (equal? p1 p2) 
    frame
    (var? p1) 
    (extend-if-possible p1 p2 frame)
    (var? p2) 
    (extend-if-possible p2 p1 frame) ; ***
    (and (pair? p1) (pair? p2))
    (unify-match (cdr p1)
                 (cdr p2)
                 (unify-match (car p1);RECUR!!!!!!!!!!!!!!!
                              (car p2)
                              frame))
    :else 'failed
    )
  )

(defn rename-variables-in
  "the program renames all the variables in the rule with unique new names. The
reason for this is to prevent the variables for different rule applications from becoming confused with each other.
For instance, if two rules both use a variable named ?x, then each one may add a binding for ?x to the frame when
it is applied. These two ?x's have nothing to do with each other, and we should not be fooled into thinking that the
two bindings must be consistent. Rather than rename variables, we could devise a more clever environment
structure; however, the renaming approach we have chosen here is the most straightforward, even if not the most
efficient. (See exercise 4.79.)
We generate unique variable names by associating a unique identifier (such as a number) with each rule application
and combining this identifier with the original variable names. For example, if the rule-application identifier is 7,
we might change each ?x in the rule to ?x-7 and each ?y in the rule to ?y-7. (Make-new-variable and new-ruleapplication-
id are included with the syntax procedures in section 4.4.4.7.)"
  [rule]
  (let [rule-application-id (new-rule-application-id)]
    (letfn [(tree-walk 
              [exp]
              (cond (variable? exp)
                    (make-new-variable exp rule-application-id)
                    (pair? exp)
                    (cons-pair 
                      (tree-walk (car exp));RECUR!!!!!!!!!!!!!!!!!!!!
                      (tree-walk (cdr exp)));RECUR!!!!!!!!!!!!!!!!!!!!
                    :else exp
                    ))]
    (tree-walk rule)))
  )

(defn apply-a-rule 
  "Apply-a-rule applies rules using the method outlined in section 4.4.2. It first augments its argument frame by
unifying the rule conclusion with the pattern in the given frame. If this succeeds, it evaluates the rule body in this
new frame."
  [rule query-pattern query-frame]
  (let [clean-rule (rename-variables-in rule)]
    (let [unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)]
      (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule) (singleton-stream unify-result)))))
  )

(defn apply-rules
  "Apply-rules is the rule analog of find-assertions (section 4.4.4.3). It takes as input a pattern and a frame, and it
forms a stream of extension frames by applying rules from the data base. Stream-flatmap maps apply-a-rule
down the stream of possibly applicable rules (selected by fetch-rules, section 4.4.4.5) and combines the resulting
streams of frames."
  [pattern frame]
  (stream-flatmap 
    (fn [rule] (apply-a-rule rule pattern frame))
    (fetch-rules pattern frame))
  )

;---------------------------------------------------------------------------------------------------
; REST

(defn simple-query [query-pattern frame-stream]
  (stream-flatmap
    (fn [frame]
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream)
  )

(defn qeval [query frame-stream]
  (let [query-type (exp-type query)
        qproc (get-from-table query-type 'qeval)]
    (if qproc
      (qproc (exp-contents query) frame-stream)
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

