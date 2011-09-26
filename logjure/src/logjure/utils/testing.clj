(ns logjure.utils.testing)

(defn refer-private [ns] 
  (doseq [[symbol var] (ns-interns ns)] 
    (when (:private (meta var)) 
      (intern *ns* symbol var)))) 

