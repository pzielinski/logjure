;env as list of maps/frames of vars values, perf slower 1.04 times, memory 2.45GB for arithmetic series 1M
(ns scheme.env.env-map-list)

(use 'scheme.env.utils)

(defn the-empty-environment 
  [] 
  '({})
  )

(defn empty-environment? 
  [env] 
  (empty? (first env))
  )

(defn 
  set-variable-value-in-env 
  [variable value env]
  (cons (assoc (first env) variable value) (rest env))
  )

(defn 
  lookup-variable-value-in-env
  [variable env]
  (let [value (get (first env) variable)]
    (if (not (nil? value)) 
      value 
      (if (empty? (rest env)) 
        nil 
        (recur variable (rest env)))))
  )

(defn extend-environment 
  [variables values env] 
  (if (and (empty? variables) (empty? values))
    env
    (if (= (count variables) (count values))
      (let [kvs (interleave variables values)]
        (cons (apply assoc {} kvs) env))
      (error "Variables/values counts do not match!" (list variables values))))
  )

(defn extend-environment-with-map 
  [map env]
  (cons map env)
  )
