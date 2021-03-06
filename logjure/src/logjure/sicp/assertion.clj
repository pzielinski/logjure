(ns logjure.sicp.assertion
  (:use 
    logjure.utils.treenode
    logjure.utils.treeseq
    logjure.utils.pair
    logjure.utils.stream
    logjure.sicp.table
    logjure.sicp.syntax
    logjure.sicp.frame
    logjure.sicp.store
    )
  )

;---------------------------------------------------------------------------------------------------
; MATCH ASSERTIONS

(defn- pattern-match-seq
  ([pat dat frame]
    (pattern-match-seq (tree-seq-multi-depth-leaves pat dat) frame))
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
                (pattern-match-seq 
                  (concat (tree-seq-multi-depth-leaves n1-var-value n2) (rest s))
                  frame)))
            ;NO value for n1 variable in frame
            (let [new-frame (extend-frame n1 n2 frame)]
              (cons
                [n1 n2 new-frame]
                (lazy-seq
                  (pattern-match-seq (rest s) new-frame))
                )))
          ;n1 is not a variable
          (cons
            [n1 n2 frame]
            (lazy-seq
              (pattern-match-seq (rest s) frame)))
          ))))
  )  

(defn pattern-match
  "The basic pattern matcher returns either the symbol failed or an extension of the given frame. The basic idea of
the matcher is to check the pattern against the data, element by element, accumulating bindings for the pattern
variables. If the pattern and the data object are the same, the match succeeds and we return the frame of bindings
accumulated so far. Otherwise, if the pattern is a variable we extend the current frame by binding the variable to the
data, so long as this is consistent with the bindings already in the frame. If the pattern and the data are both pairs,
we (recursively) match the car of the pattern against the car of the data to produce a frame; in this frame we then
match the cdr of the pattern against the cdr of the data. If none of these cases are applicable, the match fails and we
return the symbol failed.

extend-if-consistent:
Here is the procedure that extends a frame by adding a new binding, if this is consistent with the bindings already in
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
  ([pat dat frame]
    ;already failed - stop
    (if (or (equal? frame 'failed) (not pat) (not dat))
      'failed
      (let [s (pattern-match-seq pat dat frame)
            nomatch? (fn nomatch? [[n1 n2 _]] (and (not (variable? n1)) (not (equal? n1 n2))))]
        (if (some nomatch? s)
          'failed
          (get (last s) 2)))))
  )

(defn check-an-assertion
  "Check-an-assertion takes as arguments a pattern, a data object (assertion), and a frame and returns either a oneelement
stream containing the extended frame or the-empty-stream if the match fails."
  [assertion query-pat query-frame]
  (let [match-result (pattern-match query-pat assertion query-frame)]
    (when (not (eq? match-result 'failed))
      (list match-result)))
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
  (mapcat 
    (fn [datum] (check-an-assertion datum pattern frame))
    (get-assertions pattern frame))
  )

