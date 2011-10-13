(ns logjure.sicp.rule
  (:use
    logjure.utils.defmultimethod
    logjure.utils.treenode
    logjure.utils.treeseq
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
  (symbol (str variable rule-application-id))
  )

(defn- depends-on-seq
  ([exp the-var frame]
    (depends-on-seq (tree-seq-depth exp) exp the-var frame))
  ([the-seq exp the-var frame]
    (when (seq the-seq)
      (if-let [e (first the-seq)]
        (if (variable? e)
          ;e is variable
          (if (equal? the-var e)
            (cons
              [e true frame]
              (lazy-seq
                (depends-on-seq (rest the-seq) exp the-var frame)))
            (if-let [value (get-value-in-frame e frame)]
              ;there is a value for e variable in frame
              (cons
                [e false frame]
                (lazy-seq
                  (depends-on-seq 
                    (concat (tree-seq-depth value) (rest the-seq))
                    exp
                    the-var
                    frame)))
              ;NO value for e variable in frame
              (cons
                [e false frame]
                (lazy-seq
                  (depends-on-seq (rest the-seq) exp the-var frame))
                )))
          ;e is not a variable
          (cons
            [e false frame]
            (lazy-seq
              (depends-on-seq (rest the-seq) exp the-var frame)))
          ))))
  )  

(defn depends-on?
  "Depends-on? is a predicate that tests whether an expression proposed to be the value of a pattern variable depends
on the variable. This must be done relative to the current frame because the expression may contain occurrences of
a variable that already has a value that depends on our test variable. The structure of depends-on? is a simple
recursive tree walk in which we substitute for the values of variables whenever necessary."
  [exp the-var frame]
  (if (some (fn [[_ depends-on _]] depends-on) (depends-on-seq exp the-var frame)) true false)
  )

(defmultimethod replace-symbol 
  [x oldsym newsym]
  (fn [x oldsym newsym] (if (coll? x) :collection :scalar))
  :collection 
  (lazy-seq
    (when (seq x)
      (cons (replace-symbol (first x) oldsym newsym)
            (replace-symbol (rest x) oldsym newsym))))
  :scalar
  (if (= x oldsym) newsym x)  
  )

(defmultimethod replace-symbol 
  [x oldsym newsym]
  (fn [x oldsym newsym] (if (coll? x) :collection :scalar))
  :collection 
  (lazy-seq
    (when (seq x)
      (cons (replace-symbol (first x) oldsym newsym)
            (replace-symbol (rest x) oldsym newsym))))
  :scalar
  (if (= x oldsym) newsym x)  
  )

(defmultimethod resolve-variables 
  [x frame]
  (fn [x frame] (if (coll? x) :collection :scalar))
  :collection 
  (lazy-seq
    (when (seq x)
      (cons (resolve-variables (first x) frame)
            (resolve-variables (rest x) frame))))
  :scalar
  (if (variable? x)
    (if-let [value (get-value-in-frame x frame)]
      (recur value frame)
      x)
    x)
  )

(defn- unify-match-seq
  "Using tree-seq-multi-depth only quarantees that pat & dat can not both be a sequences. 
It is possible that one of pat or dat can still be a sequence."
  ([pat dat frame]
    ;already failed - stop
    (if (or (equal? frame 'failed) (not pat) (not dat))
      (list [pat dat 'failed])
      (unify-match-seq (tree-seq-multi-depth-leaves pat dat) frame)))
  ([s frame]
    (when (seq s)
      (let [[n1 n2] (first s)]
        (if (variable? n1)
          ;n1 is variable
          (if-let [n1-var-value (get-value-in-frame n1 frame)]
            ;there is a value for n1 variable in frame
            (cons
              [n1 n2 frame]
              (lazy-seq
                (unify-match-seq 
                  (concat (tree-seq-multi-depth-leaves n1-var-value n2) (rest s))
                  frame)))
            ;NO value for n1 variable in frame
            ;n2 can still be an expression that depends on n1, hence depends-on? check
            (if (depends-on? n2 n1 frame)
              ;n2 expr depends on n1 variable - stop & FAIL the whole thing
              (list [n1 n2 'failed])
              ;n2 expr does not depend on n1 variable - can safely extend frame
              (let [n2-resolved (resolve-variables n2 frame)
                    new-frame (extend-frame n1 n2-resolved frame)]
                (cons
                  [n1 n2 new-frame]
                  (lazy-seq
                    (unify-match-seq (rest s) new-frame))
                  ))
              ))
          ;n1 is not a variable - check if n2 is variable
          (if (variable? n2)
            ;n2 is variable (n1 is not a variable)
            (if-let [n2-var-value (get-value-in-frame n2 frame)]
              ;there is a value for n2 variable in frame
              (cons
                [n1 n2 frame]
                (lazy-seq
                  (unify-match-seq 
                    (concat (tree-seq-multi-depth-leaves n1 n2-var-value) (rest s))
                    frame)))
              ;NO value for n2 variable in frame (n1 is not a variable): extend frame for n2 var with n1 value
              ;n1 can still be an expression (not a variable) that depends on n2, hence depends-on? check
              (if (depends-on? n1 n2 frame)
                ;n1 expr depends on n2 variable - stop & FAIL the whole thing
                (list [n1 n2 'failed])
                ;n1 expr does not depend on n2 variable - can safely extend frame
                (let [n1-resolved (resolve-variables n1 frame)
                      new-frame (extend-frame n2 n1 frame)]
                  (cons
                    [n1 n2 new-frame] 
                    (lazy-seq
                      (unify-match-seq (rest s) new-frame))
                    ))))
            ;n2 is not a variable (n1 is not a variable)
            (cons
              [n1 n2 frame]
              (lazy-seq
                (unify-match-seq (rest s) frame))))
          ))))
  )  

(defn unify-match
  "The unification algorithm is implemented as a procedure that takes as inputs two patterns and a frame and returns
either the extended frame or the symbol failed. The unifier is like the pattern matcher except that it is symmetrical
-- variables are allowed on both sides of the match. Unify-match is basically the same as pattern-match, except
that there is extra code (marked ``***'' below) to handle the case where the object on the right side of the match is a
variable.
  In unification, as in one-sided pattern matching, we want to accept a proposed extension of the frame only if it is
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
  ([p1 p2 frame]
    (letfn 
      [(nomatch? 
         [[n1 n2 frame]]
         (or
           ;if both are not variables and not equal
           (and (not (variable? n1)) (not (variable? n2)) (not (equal? n1 n2)))
           ;frame can be 'failed if depends-on? failed
           (= frame 'failed)))]
      (let [s (unify-match-seq p1 p2 frame)]
        (if (some nomatch? s)
          'failed
          (get (last s) 2)))))
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
                    (seq? exp)
                    (cons 
                      (tree-walk (first exp));RECUR!!!!!!!!!!!!!!!!!!!!
                      (tree-walk (second exp)));RECUR!!!!!!!!!!!!!!!!!!!!
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
      (when (not (eq? unify-result 'failed))
        (qeval (rule-body clean-rule) (list unify-result)))))
  )

(defn apply-rules
  "Apply-rules is the rule analog of find-assertions (section 4.4.4.3). It takes as input a pattern and a frame, and it
forms a stream of extension frames by applying rules from the data base. Stream-flatmap maps apply-a-rule
down the stream of possibly applicable rules (selected by fetch-rules, section 4.4.4.5) and combines the resulting
streams of frames."
  [pattern frame qeval]
  (mapcat 
    (fn [rule] (apply-a-rule rule pattern frame))
    (get-rules pattern frame))
  )
