(ns logjure.utils.sequtil
  (:use 
    logjure.utils.lazytree
    logjure.utils.treeseq
    logjure.utils.testing 
    clojure.contrib.test-is
    )
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

(defn deep-equal?
  "Compares s forms."
  [expr1 expr2]
  (every? 
    (fn [[v1 v2]] (= v1 v2)) 
    (tree-seq-depth (tree-disjoin (seq-tree expr1) (seq-tree expr2) :not-found)))
  )