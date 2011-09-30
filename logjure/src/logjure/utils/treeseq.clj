(ns logjure.utils.treeseq
  (:use logjure.utils.defmultimethod)
  (:require clojure.inspector))

(defprotocol TreeNode
  (node-is-leaf [node])
  (node-get-children [node])
  (node-get-value [node])
  )

(defrecord TreeNodeFixed [value children])

(extend-type TreeNodeFixed
  TreeNode
  (node-is-leaf [node] (nil? (node-get-children node)))
  (node-get-children [node] (:children node))
  (node-get-value [node] (:value node))
)

(defn finalize-tree-node-dynamic-do-nothing [tree-node-dynamic])

(def *finalize-tree-node-dynamic-fn* (atom finalize-tree-node-dynamic-do-nothing))

(deftype TreeNodeDynamic [child-value-seq]
  Cloneable
  (finalize 
    [this] 
    (@*finalize-tree-node-dynamic-fn* this))
  )

(defn create-tree-node-dynamic [child-value-seq]
  (TreeNodeDynamic. child-value-seq)
  )

(extend-type TreeNodeDynamic 
  TreeNode
  (node-is-leaf 
    [node] 
    (let [value (node-get-value node)]
          (or (nil? value) (not (sequential? value)))))
  (node-get-children 
    [node]
    (if (node-is-leaf node)
      ()
      (let [value (node-get-value node)]
        (map #(create-tree-node-dynamic %) value))))
  (node-get-value 
    [node] 
    (.child-value-seq node))
)

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
  ([] (lazy-seq nil))
  ([col cols]
    (lazy-seq
      (let [s (seq col)]
        (if s
          (cons (first s) (lazy-list-merge (rest s) cols))
          (if (not (empty? cols))
            (lazy-list-merge (first cols) (rest cols)))))))
  ([cols]
       (lazy-list-merge (first cols) (rest cols)))
)

(defn get-nodes-at-depth-series
  "Returns a lazy sequence of lazy sequences. 
   Eeach child sequence contains all nodes at corresponding depth."
   ([branch? children root]
     (let [walk 
           (fn walk [branch? children parent-nodes]
             (lazy-seq
               (let [nodes (lazy-list-merge (map #(children %) parent-nodes))]
                 (when (not (empty? nodes))
                   (cons nodes
                         (walk branch? children nodes))))))]
       (lazy-seq
         (let [root-list (list root)]
           (cons root-list 
                 (walk branch? children root-list)))))
     )
   ([root] 
     (get-nodes-at-depth-series #(not (is-leaf %)) get-children root)
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

;stops walking at allowed depth
(defn get-nodes-at-depth
   ([branch? children root allowed-depth]
   (let [walk 
         (fn walk [node depth]
           (lazy-seq
             (if (< depth allowed-depth)
               (when (branch? node)
                 (mapcat #(walk % (inc depth)) (children node)))
               (list node))
             ))]
     (walk root 0)))
   ([root allowed-depth] 
     (get-nodes-at-depth #(not (is-leaf %)) get-children root allowed-depth))
)

(defn tree-seq-multi-depth
  "Walks two trees in lockstep."
   ([root1 root2]
     (tree-seq-multi-depth #(not (is-leaf %)) get-children root1 root2))
   ([is-branch? get-children root1 root2]
     (letfn [(walk 
               [node1 node2]
               (lazy-seq
                 (if
                   (and (is-branch? node1) (is-branch? node2))
                   (cons [node1 node2]
                         ;ISSUE: map silently ignores some children if one seq is longer !!!!!!!
                         (mapcat walk (get-children node1) (get-children node2)))
                   (list [node1 node2])
                   )))]
            (walk root1 root2)))
   )
