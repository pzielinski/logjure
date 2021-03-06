(ns logjure.sicp.qrepl
  (:use 
    logjure.sicp.syntax
    logjure.sicp.table
    logjure.sicp.frame
    logjure.sicp.store
    logjure.sicp.assertion
    logjure.sicp.rule
    logjure.sicp.qeval
    )
  )

(def input-prompt ";;; Query input:")
(def output-prompt ";;; Query results:")

(defn prompt-for-input [input-prompt]
  (println input-prompt)
  )

(defn read-input []
  (read-line)
  )

(defn query-driver-loop []
  (prompt-for-input input-prompt)
  (let [q (read-input)]
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
            (map
              #(println %)
              (map
                (fn [frame] (instantiate q frame))
                (qeval q (list (make-empty-frame)))))
            (query-driver-loop))
          )))

