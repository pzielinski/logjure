(ns logjure.utils.testing)

(defn refer-private [ns] 
  (doseq [[symbol var] (ns-interns ns)] 
    (when (:private (meta var)) 
      (intern *ns* symbol var)))) 

(defn deeply-nested
  ([n]
    (deeply-nested n '(:bottom)))
  ([n bottom]
  (loop [n n result bottom]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))
  )

(defmacro recorder [original-fn replacement-fn & test-fn]
  "Calls evaluates test-fn in binding where original-fn is replaced by replacement-fn 
and argument for each original-fn call is recoreded in a vector, 
the result is a vector of test-fn result and vector of orignal-fn arguments.
Example: 
(recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 0))
produces result:
[1 [1 2 3]]"
  `(let [records# (atom [])
        record-fn# (fn [record#] (swap! records# conj record#))
        f-recording# (fn [x#] (do (record-fn# x#)) (~replacement-fn x#))
        ]
    (binding [~original-fn f-recording#]
      (let [result# ~@test-fn]
        [result# @records#])
    ))
  )
