(ns logjure.utils.treeseq
  (:use 
    logjure.utils.defmultimethod 
    logjure.utils.lazytree
    logjure.sicp.stream
    )
  (:require clojure.inspector))

;lazy
(defn get-child-seq 
  ([node indx is-leaf get-child-count get-child]
    (lazy-seq 
      (if (is-leaf node)
        ()
        (let [child-count (get-child-count node)]
          (if (= child-count 0)
            ()
            (cons (get-child node indx)
                  (when (< indx (dec child-count))
                    (get-child-seq node (inc indx) is-leaf get-child-count get-child))
                  ))))))
  ([node indx] 
    (get-child-seq 
      node 
      indx 
      clojure.inspector/is-leaf 
      clojure.inspector/get-child-count 
      clojure.inspector/get-child))
  ([node] 
    (get-child-seq node 0))
)

(defmultimethod is-leaf [node] #(satisfies? TreeNode %)
  true (node-is-leaf node)
  :default (clojure.inspector/is-leaf node)
  )

(defmultimethod get-children [node] #(satisfies? TreeNode %)
  true (node-get-children node)
  :default (get-child-seq node)
  )

(defn tree-seq-depth
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
   [root]
   (tree-seq #(not (is-leaf %)) get-children root))

(defn lazy-list-merge
  [cols]
  (let [cat 
        (fn cat [col cols]
          (lazy-seq
            (let [s (seq col)]
              (if s
                (cons (first s) (cat (rest s) cols))
                (when cols
                  (cat (first cols) (next cols)))))))] 
    (cat (first cols) (next cols)))
  )

(defn lazy-list-merge-x
  "Makes get-nodes-at-depth-series faile with StackOverflow error."
  ([] (lazy-seq nil))
  ([col cols]
    (lazy-seq
      (let [s (seq col)]
        (if s
          (cons (first s) (lazy-list-merge-x (rest s) cols))
          (if (not (empty? cols))
            (lazy-list-merge-x (first cols) (rest cols)))))))
  ([cols]
    (lazy-seq 
      (lazy-list-merge-x (first cols) (rest cols))))
  )

(defn get-nodes-at-depth-series
  "Returns a lazy sequence of lazy sequences. 
   Eeach child sequence contains all nodes at corresponding depth."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parent-nodes]
             (cons parent-nodes
                   (when (seq parent-nodes)
                     (lazy-seq 
                       (let [child-nodes (lazy-list-merge (map get-children parent-nodes))]
                         (walk branch? get-children child-nodes))))))]
       (walk branch? get-children (list root)))
     )
   ([root] 
     (get-nodes-at-depth-series #(not (is-leaf %)) get-children root)
     )
)

(defn get-nodes-at-depth-series-x
  "Returns a lazy sequence of lazy sequences. 
   Eeach child sequence contains all nodes at corresponding depth."
   ([branch? get-children root]
     (let [walk 
           (fn walk 
             [branch? get-children parent-nodes]
               (cons parent-nodes
                     (when (seq parent-nodes)
                       (lazy-seq 
                         (walk branch? get-children (lazy-list-merge-x (map get-children parent-nodes)))))))]
       (lazy-seq
           (walk branch? get-children (list root))))
     )
   ([root] 
     (get-nodes-at-depth-series-x #(not (is-leaf %)) get-children root)
     )
)

(defn tree-seq-breadth 
  "Returns a lazy sequence of the nodes in a tree, via a breadth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
   tree.
   Keeps in mem all nodes from previous level.
   "
  ([branch? children root]
    (lazy-list-merge (get-nodes-at-depth-series branch? children root)))
  ([root]
    (tree-seq-breadth #(not (is-leaf %)) get-children root))
  )

(defn tree-seq-breadth-x 
  "Uses lazy-list-merge-x. Is most lazy of all tree-seq-breadth-*."
  ([branch? children root]
    (lazy-list-merge-x (get-nodes-at-depth-series-x branch? children root)))
  ([root]
    (tree-seq-breadth-x #(not (is-leaf %)) get-children root))
  )

(defn tree-stream-breadth
  "Will get stuck if node has infinite children."
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

(defn tree-stream-breadth-seq
  "Converts to sequence, makes it more greedy."
   ([branch? get-children root]
     (stream-to-seq (tree-stream-breadth branch? get-children root)))
   ([root]
     (stream-to-seq (tree-stream-breadth root)))
)

(defn tree-stream-interleave
  "The only tree-seq capable of sequencing infinite tree, where each node has infinite children."
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

(defn tree-stream-interleave-seq
  "Converts to sequence, makes it more greedy."
   ([branch? get-children root]
     (stream-to-seq (tree-stream-interleave branch? get-children root)))
   ([root]
     (stream-to-seq (tree-stream-interleave root)))
)

;stops walking at allowed depth
(defn get-nodes-at-depth
   ([branch? children root allowed-depth]
   (let [walk 
         (fn walk [node depth]
           (lazy-seq
             (if (< depth allowed-depth)
               (when (branch? node)
                 ;(lazy-list-merge (map #(walk % (inc depth)) (children node)))) ;StackOverflow error on 10000
                 (mapcat #(walk % (inc depth)) (children node)))
               (list node))
             ))]
     (walk root 0)))
   ([root allowed-depth] 
     (get-nodes-at-depth #(not (is-leaf %)) get-children root allowed-depth))
   )

(defn seq-to-tree
  [s]
  (seq-tree s is-leaf get-children)
  )

(defn tree-seq-multi-depth
  "Walks two trees in lockstep."
   ([root1 root2]
     (let [root1x (if (satisfies? TreeNode root1) root1 (seq-tree root1 is-leaf get-children))
           root2x (if (satisfies? TreeNode root2) root2 (seq-tree root2 is-leaf get-children))]
     (tree-seq-multi-depth 
       (tree-seq-depth (tree-map-id root1x)) 
       (tree-seq-depth (tree-map-id root2x)) 
       nil
       nil)))
   ([s1 s2 previous-n1 previous-n2]
     (when (and (seq s1) (seq s2))
       (let [n1 (first s1)
             n2 (first s2)
             id1 (:id n1)
             id2 (:id n2)
             [n1x s1x n2x s2x]
             (cond 
               (= id1 id2) 
               [n1 s1 n2 s2]
               (and (is-leaf previous-n1) (not (is-leaf previous-n2)))
               (let [new-s2 (drop-while #(not (= id1 (:id %))) s2)
                     new-n2 (first new-s2)]
                 [n1 s1 new-n2 new-s2])
               (and (not (is-leaf previous-n1)) (is-leaf previous-n2))
               (let [new-s1 (drop-while #(not (= id2 (:id %))) s1)
                     new-n1 (first new-s1)]
                 [new-n1 new-s1 n2 s2])
               )
             ]
         (cons
           [(node-get-value n1x) (node-get-value n2x)]
           (lazy-seq
             (tree-seq-multi-depth 
               (rest s1x) 
               (rest s2x) 
               n1x
               n2x))))))
   )
