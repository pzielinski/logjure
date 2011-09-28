(ns logjure.utils.testing)

(defn refer-private [ns] 
  (doseq [[symbol var] (ns-interns ns)] 
    (when (:private (meta var)) 
      (intern *ns* symbol var)))) 

(defn deeply-nested [n]
  (loop [n n result '(:bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))
