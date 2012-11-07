(ns scheme.env.env)

(use 'scheme.env.utils)

(defn the-empty-environment 
  [] 
  '{})

(defn empty-environment? 
  [env] 
  (empty? env))

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
    (if (and (not (empty? variables)) (not (empty? values)))
      (let [first-variable (first variables)
            first-value (first values)
            rest-variables (rest variables)
            rest-values (rest values)
            new-env (set-variable-value-in-env first-variable first-value env)]
      (recur rest-variables rest-values new-env))
      (error "Variables/values counts do not match!" (list variables values))
      )
    )
  )

(defn extend-environment-with-map 
  [map env]
  (merge env map)
  )
