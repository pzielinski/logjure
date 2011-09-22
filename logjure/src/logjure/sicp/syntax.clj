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

