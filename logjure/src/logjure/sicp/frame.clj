(ns logjure.sicp.frame
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    )
  )

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

