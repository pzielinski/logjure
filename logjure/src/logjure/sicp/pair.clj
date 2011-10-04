(ns logjure.sicp.pair
  )

(defn null? [x]
  (nil? x)
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

