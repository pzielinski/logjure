(ns logjure.sicp.syntax
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    )
  )

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

(defn tagged-list? [exp tag]
  (if (pair? exp)
    (eq? (car exp) tag)
    false)
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

(defn contract-question-mark [variable]
  (string->symbol
    (string-append "?"
                   (if (pair? (cdr variable));FIXED BUG !!! was: (number? (cadr variable))
                     (string-append (symbol->string (cddr variable));FIXED BUG !!! was caddr
                                    "-"
                                    (number->string (cadr variable)))
                     (symbol->string (cdr variable)))));FIXED BUG !!! was: (cadr variable)
  )

