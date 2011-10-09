(ns logjure.utils.sequtil)

(map-reduce
  [f coll result]
  (lazy-seq
    (when-let 
      [s (seq coll)
       res (f (first s) result)]
      (cons res (map-reduce f (rest s) res))))
  )

