(ns logjure.utils.treeseq
  (:use 
    logjure.utils.defmultimethod 
    logjure.utils.treenode
    logjure.utils.lazytree
    )
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

(defn ^:dynamic tree-seq-breadth 
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

(defn tree-seq-multi-depth
  "Walks two trees in lockstep. Extract all nodes values into sequence, depth first."
  [root1 root2]
  (let [root1x (if (satisfies? TreeNode root1) root1 (seq-tree root1))
        root2x (if (satisfies? TreeNode root2) root2 (seq-tree root2))]
    (map
      #(:value %)
      (tree-seq-depth (tree-conjoin root1x root2x))))
  )

(defn tree-seq-multi-depth-leaves
  "Walks two trees in lockstep. Extract leaf nodes values into sequence, depth first."
  [root1 root2]
  (let [root1x (if (satisfies? TreeNode root1) root1 (seq-tree root1))
        root2x (if (satisfies? TreeNode root2) root2 (seq-tree root2))]
    (map
      #(:value %)
      (filter
        node-is-leaf
        (tree-seq-depth (tree-conjoin root1x root2x)))))
  )
