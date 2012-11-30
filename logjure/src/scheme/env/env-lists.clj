;env as map of vars values lists, slower 1.37 times, memory 2.7GB for arithmetic series 1M
(ns scheme.env.env)

(use 'scheme.env.utils)

(defn the-empty-environment 
  [] 
  {:variables '() :values '()}
  )

(defn empty-environment? 
  [env] 
  (empty? (:variables env))
  )

(defn 
  get-variable-names
  [env]
  (:variables env)
  )

(defn 
  set-variable-value-in-env 
  [variable value env]
  {:variables (cons variable (:variables env)) :values (cons value (:values env))}
  )

(defn 
  index-of 
  [e coll] 
  (first (keep-indexed #(if (= e %2) %1) coll))
  )

(defn 
  lookup-variable-value-in-env
  [variable env]
  (let [index (index-of variable (:variables env))]
    (if (nil? index)
      nil
      (nth (:values env) index)))
  )

(defn extend-environment 
  [variables values env] 
  (if (and (empty? variables) (empty? values))
    env
    (if (= (count variables) (count values))
      (reduce (fn [env [var val]] (set-variable-value-in-env var val env)) env (map (fn [var val] [var val]) variables values))
      (error "Variables/values counts do not match!" (list variables values))))
  )

(defn extend-environment-with-map 
  [map env]
  (extend-environment (keys map) (vals map) env)
  )
