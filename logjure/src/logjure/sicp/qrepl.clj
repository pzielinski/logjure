(ns logjure.sicp.qrepl
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    logjure.sicp.table
    logjure.sicp.syntax
    logjure.sicp.frame
    logjure.sicp.store
    logjure.sicp.assertion
    logjure.sicp.rule
    logjure.sicp.qeval
    )
  )

;---------------------------------------------------------------------------------------------------
; REPEL

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
  (let [q (query-syntax-process (read-input))]
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
                (fn [frame]
                        (instantiate q
                                     frame
                                     (fn [v f]
                                             (contract-question-mark v))))
                (qeval q (singleton-stream (make-empty-frame)))))
            (query-driver-loop))
          )))

