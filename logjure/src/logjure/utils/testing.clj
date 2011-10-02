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
  `(let [records# (atom #{})
        record-fn# (fn [record#] (swap! records# conj record#))
        f-recording# (fn [x#] (do (record-fn# x#)) (~replacement-fn x#))
        ]
    (binding [~original-fn f-recording#]
      [~@test-fn @records#]
    ))
  )