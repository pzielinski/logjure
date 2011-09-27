(ns logjure.sicp.syntax
  (:use 
    logjure.sicp.base 
    )
  )

(defn exp-type [exp]
  (if (seq? exp)
    (first exp)
    (error "Unknown expression TYPE" exp))
  )

(defn exp-contents [exp]
  (if (seq? exp)
    (second exp)
    (error "Unknown expression CONTENTS" exp))
  )

(defn tagged-list? [exp tag]
  (if (seq? exp)
    (eq? (first exp) tag)
    false)
  )

(defn variable? [exp]
  (= \? (first (str exp)))
  )

(defn constant-symbol? [exp] 
  (and (symbol? exp) (not (variable? exp)))
  )

(defn assertion-to-be-added? [exp]
  (eq? (exp-type exp) 'assert!)
  )

(defn add-assertion-body [exp]
  (first (exp-contents exp))
  )

(defn empty-conjunction? [exps] 
  (null? exps)
  )

(defn first-conjunct [exps] 
  (first exps)
  )

(defn rest-conjuncts [exps] 
  (second exps)
  )

(defn empty-disjunction? [exps] 
  (null? exps)
  )

(defn first-disjunct [exps] 
  (first exps)
  )

(defn rest-disjuncts [exps] 
  (second exps)
  )

(defn negated-query [exps] 
  (first exps)
  )

(defn predicate [exps] 
  (first exps)
  )

(defn args [exps] 
  (second exps)
  )

(defn rule? [statement]
  (tagged-list? statement 'rule)
  )

(defn conclusion [rule] 
  (second rule)
  )

(defn rule-body [rule]
  (if (null? (nth rule 2 nil))
    '(always-true)
    (nth rule 2))
  )

