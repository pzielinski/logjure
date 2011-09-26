(ns logjure.sicp.frame
  (:use 
    logjure.sicp.base 
    )
  )

(defn make-empty-frame []
  {}
  )

(defn make-binding [variable value]
  (cons variable value)
  )

(defn binding-variable [binding]
  (first binding)
  )

(defn binding-value [binding]
  (second binding)
  )

(defn binding-in-frame [variable frame]
  (get frame variable)
  )

(defn extend-frame [variable value frame]
  (assoc frame variable (make-binding variable value)))

