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
  (tagged-list? exp '?)
  )

(defn constant-symbol? [exp] 
  (symbol? exp)
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

(defn expand-question-mark [symbol]
  (let [chars (symbol->string symbol)]
    (if (string=? (substring chars 0 1) "?")
      (cons '? (string->symbol (substring chars 1 (string-length chars))))
      symbol))
  )

(defn map-over-symbols [proc exp]
  (cond (seq? exp) 
        (cons 
          (map-over-symbols proc (first exp));NEED RECUR !!!!!!!!!!!!!
          (map-over-symbols proc (second exp)));NEED RECUR !!!!!!!!!!!!!
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
  (let [var-symbol-or-num-symbol-seq (second variable)]
    (string->symbol
      (string-append "?"
                     (if (seq? var-symbol-or-num-symbol-seq)
                       (string-append (symbol->string (first var-symbol-or-num-symbol-seq))
                                      "-"
                                      (number->string (second var-symbol-or-num-symbol-seq)))
                       (symbol->string var-symbol-or-num-symbol-seq)))))
  )

