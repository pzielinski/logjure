;env as list of maps/frames of vars values, baseline perf, memory 2.55GB for arithmetic series 1M
(ns scheme.env.env)

(use 'scheme.env.utils)

(defn the-empty-environment 
  [] 
  '{}
  )

(defn empty-environment? 
  [env] 
  (empty? env)
  )

(defn 
  get-variable-names
  [env]
  (keys env)
  )

(defn 
  set-variable-value-in-env 
  [variable value env]
  (assoc env variable value)
  )

(defn 
  lookup-variable-value-in-env
  [variable env]
  (get env variable)
  )

(defn extend-environment 
  [variables values env] 
  (if (and (empty? variables) (empty? values))
    env
    (if (= (count variables) (count values))
      (let [kvs (interleave variables values)]
        ;(persistent! (apply assoc! (transient env) kvs)))
        (apply assoc env kvs))
      (error "Variables/values counts do not match!" (list variables values))))
  )

(defn extend-environment-with-map 
  [map env]
  (merge env map)
  )
