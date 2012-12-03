;env as map of vars values lists, slower 1.46 times then env, memory 3.0GB for arithmetic series 1M
(ns scheme.env.env-vect-list)

(use 'scheme.env.utils)

(defn the-empty-environment 
  [] 
  '()
  )

(defn empty-environment? 
  [env] 
  (empty? env)
  )

(defn 
  set-variable-value-in-env 
  [variable value env]
  (cons [variable value] env)
  )

(defn 
  lookup-variable-value-in-env
  [variable env]
  (get (first (filter (fn[[var val]] (= variable var)) env)) 1)
  )

(defn extend-environment 
  [variables values env] 
  (if (and (empty? variables) (empty? values))
    env
    (if (= (count variables) (count values))
      (concat (map (fn[var val] [var val]) variables values) env)
      (error "Variables/values counts do not match!" (list variables values))))
  )

(defn extend-environment-with-map 
  [map env]
  (extend-environment (keys map) (vals map) env)
  )
