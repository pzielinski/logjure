(ns logjure.sicp.rule
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    logjure.sicp.table
    logjure.sicp.syntax
    logjure.sicp.frame
    logjure.sicp.store
    )
  )

; MATCH RULES

(def *rule-counter* (atom 0))

(defn new-rule-application-id []
  (swap! *rule-counter* inc)
  )

(defn make-new-variable [variable rule-application-id]
  (cons-pair '? (cons-pair rule-application-id (cdr variable)))
  )

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
  [rule query-pattern query-frame qeval]
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
  [pattern frame qeval]
  (mapcat 
    (fn [rule] (apply-a-rule rule pattern frame))
    (fetch-rules pattern frame))
  )
