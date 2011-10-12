(ns logjure.utils.sequtil
  (:use 
    logjure.utils.lazytree
    logjure.utils.treenode
    logjure.utils.treeseq
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
    #(apply = %) 
    (map 
      node-get-value 
      (filter 
        is-leaf 
        (tree-seq-depth 
          (tree-disjoin 
            (seq-tree expr1) 
            (seq-tree expr2) 
            :not-found)))))
  )