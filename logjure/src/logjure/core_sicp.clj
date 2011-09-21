(ns logjure.core-sicp)

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

;---------------------------------------------------------------------------------------------------
; NOT IMPLEMENTED

(defn prompt-for-input [input-prompt]
  )

(defn query-syntax-process [input-exp]
  )

(defn assertion-to-be-added? [exp]
  )

(defn add-assertion-body [exp]
  )

(defn add-rule-or-assertion! [x]
  )

(defn contract-question-mark [exp]
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

(defn constant-symbol? [x]
  (symbol? x)
  )

(defn variable? [x]
  (var? x)
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
  (car (cdr pair))
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

(declare extend-if-consistent)

(defn pattern-match [pat dat frame]
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

(defn extend-if-consistent [var dat frame]
  (let [binding (binding-in-frame var frame)]
    (if binding
      (pattern-match (binding-value binding) dat frame) ;TRAMPOLINE or move to pattern-match!!!!!!!!!!!!!!
      (extend-frame var dat frame)))
  )

(defn check-an-assertion [assertion query-pat query-frame]
  (let [match-result (pattern-match query-pat assertion query-frame)]
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result)))
  )

(defn find-assertions [pattern frame]
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


;---------------------------------------------------------------------------------------------------
; REST

(defn instantiate [exp frame unbound-var-handler]
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

