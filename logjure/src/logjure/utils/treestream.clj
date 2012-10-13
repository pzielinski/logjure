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
             [branch? get-children parents]
             (cons-stream 
               parents
               (if (stream-null? parents)
                 the-empty-stream
                 (let [children (flatten-stream (stream-map #(seq-to-stream (get-children %)) parents))]
                   (walk branch? get-children children)))))]
       (flatten-stream (walk branch? get-children (seq-to-stream (list root)))))
     )
   ([root] 
     (tree-stream-breadth #(not (is-leaf %)) get-children root)
     )
)

(defn tree-stream-depth-1
  "Tree to stream, depth first. Will get stuck if tree has infinite depth."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parents]
             (when (not (stream-null? parents))
               (let [first-parent (stream-car parents)]
               (cons-stream 
                 first-parent
                 (let [first-parent-children (seq-to-stream (get-children first-parent))
                       all-the-rest (flatten-stream (cons-stream first-parent-children (stream-cdr parents)))]
                       (walk branch? get-children all-the-rest))))))]
       (walk branch? get-children (seq-to-stream (list root))))
     )
   ([root] 
     (tree-stream-breadth #(not (is-leaf %)) get-children root)
     )
   )

(defn tree-stream-depth
  "Tree to stream, depth first. Will get stuck if tree has infinite depth."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parents]
             (when (not (stream-null? parents))
               (let [first-parent (stream-car parents)]
               (cons-stream 
                 first-parent
                 (let [first-parent-children (seq-to-stream (get-children first-parent))]
                   (if (not (stream-null? first-parent-children))
                     (cons-stream
                       (stream-car first-parent-children)
                       (walk branch? get-children (stream-cdr first-parent-children)))
                     (walk branch? get-children (stream-cdr parents))))))))]
       (walk branch? get-children (seq-to-stream (list root))))
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
             [branch? get-children parents]
             (cons-stream 
               parents
               (if (stream-null? parents)
                 the-empty-stream
                 (let [children (flatten-interleave-stream (stream-map #(seq-to-stream (get-children %)) parents))]
                   (walk branch? get-children children)))))]
       (flatten-interleave-stream (walk branch? get-children (seq-to-stream (list root)))))
     )
   ([root] 
     (tree-stream-interleave #(not (is-leaf %)) get-children root)
     )
   )
