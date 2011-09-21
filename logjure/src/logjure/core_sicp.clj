(ns logjure.core_sicp)

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

(defn display [str]
  (println str)
  )

(defn display-stream [stream]
  (let [item-seq (seq stream)]
    (when item-seq
      (doseq [item item-seq] (display item))))
  )

(defn prompt-for-input [input-prompt]
  )

(defn query-syntax-process [input-expr]
  )

(defn assertion-to-be-added? [expr]
  )

(defn add-assertion-body [expr]
  )

(defn add-rule-or-assertion! [x]
  )

(defn contract-question-mark [expr]
  )

(defn binding-in-frame [expr frame]
  )

(defn binding-value [expr frame]
  )

(defn instantiate [exp frame unbound-var-handler]
  (let [copy 
        (fn copy [exp]
          (cond (var? exp)
                (let [the-binding (binding-in-frame exp frame)]
                  (if the-binding
                    (copy (binding-value the-binding))
                    (unbound-var-handler exp frame)))
                (pair? exp)
                  (cons (copy (car exp)) (copy (cdr exp)))
                :else exp))]
        (copy exp))
  )

(defn singleton-stream [the-list]
  (seq the-list)
  )

(defn qeval [expr frame-stream]
  )

(defn query-driver-loop []
  (prompt-for-input input-prompt)
  (let [q (query-syntax-process (read))]
    (cond (assertion-to-be-added? q)
          (do
            (add-rule-or-assertion! (add-assertion-body q))
            (newline)
            (display "Assertion added to data base.")
            (query-driver-loop))
          :else
          (do
            (newline)
            (display output-prompt)
            (display-stream
              (map
                (fn [frame]
                        (instantiate q
                                     frame
                                     (fn [v f]
                                             (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop))
          )))
