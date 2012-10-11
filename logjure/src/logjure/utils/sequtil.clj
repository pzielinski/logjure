(ns logjure.utils.sequtil
  (:use 
    logjure.utils.lazytree
    )
)

(defn lazy-concat [l]
  (if (seq l)
    (lazy-cat (first l) (lazy-concat (next l)))
    '())
  )

(defn lazy-concat-map [f l]
  (if (seq l)
    (lazy-cat (f (first l)) (lazy-concat-map f (next l)))
    '())
  )

(defn map-reduce
  [f coll result]
  (lazy-seq
    (when-let 
      [s (seq coll)]
      (let 
        [res (f (first s) result)]
        (cons res (map-reduce f (rest s) res)))))
  )

