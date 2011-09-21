(ns utils.defmultimethod)

(defmacro defmultimethod [fn-name vargs dispatch-fn & dispatch-pairs]
  (when dispatch-pairs
     `(do 
        (defmulti ~fn-name ~dispatch-fn)
        ~@(map (fn [[dispatch-value body]] (list 'defmethod fn-name dispatch-value vargs body)) 
          (partition 2 dispatch-pairs)))))
