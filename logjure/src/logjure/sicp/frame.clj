(ns logjure.sicp.frame
  )

(defn make-empty-frame []
  {}
  )

(defn get-value-in-frame [variable frame]
  (get frame variable)
  )

(defn extend-frame [variable value frame]
  (assoc frame variable value))

