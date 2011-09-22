(ns logjure.sicp.pair
  (:use logjure.sicp.base)
  )

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
  )

(defn set-cdr! [x pair]
  (cons-pair (car pair) x)
  )

(defn assoc- [key records]
  (cond (null? records) 
        false
        (equal? key (caar records)) 
        (car records)
        :else 
        (assoc- key (cdr records)));NEED RECUR !!!!!!!!!!!!!
  )

