(ns logjure.utils.treestream
  (:use 
    logjure.utils.treenode
    logjure.utils.stream
    )
  )

(defn tree-stream-breadth
  "Tree to stream, breadth first. Will get stuck if node has infinite children."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parent-nodes]
             (cons-stream 
               parent-nodes
               (if (stream-null? parent-nodes)
                 the-empty-stream
                 (let [child-nodes (flatten-stream (stream-map #(seq-to-stream (get-children %)) parent-nodes))]
                   (walk branch? get-children child-nodes)))))]
       (flatten-stream (walk branch? get-children (seq-to-stream (list root)))))
     )
   ([root] 
     (tree-stream-breadth #(not (is-leaf %)) get-children root)
     )
)

(defn tree-stream-interleave
  "Tree to stream. The only tree-seq capable of sequencing infinite tree, where each node has infinite children."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parent-nodes]
             (cons-stream 
               parent-nodes
               (if (stream-null? parent-nodes)
                 the-empty-stream
                 (let [child-nodes (flatten-interleave-stream (stream-map #(seq-to-stream (get-children %)) parent-nodes))]
                   (walk branch? get-children child-nodes)))))]
       (flatten-interleave-stream (walk branch? get-children (seq-to-stream (list root)))))
     )
   ([root] 
     (tree-stream-interleave #(not (is-leaf %)) get-children root)
     )
   )
