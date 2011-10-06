(ns logjure.utils.treeseq
  (:use 
    logjure.utils.defmultimethod 
    logjure.sicp.stream
    )
  (:require clojure.inspector))

(defprotocol TreeNode
  (node-is-leaf [node])
  (node-get-children [node])
  (node-get-value [node])
  )

(defrecord TreeNodeX [value is-leaf get-children])

(extend-type TreeNodeX
  TreeNode
  (node-is-leaf [node] ((:is-leaf node) node))
  (node-get-children [node] ((:get-children node) node))
  (node-get-value [node] (:value node))
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

(defn tree-seq-breadth-stream
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
     (tree-seq-breadth-stream #(not (is-leaf %)) get-children root)
     )
)

(defn tree-seq-breadth-stream-seq
  "Converts to sequence, makes it more greedy."
   ([branch? get-children root]
     (stream-to-seq (tree-seq-breadth-stream branch? get-children root)))
   ([root]
     (stream-to-seq (tree-seq-breadth-stream root)))
)

(defn tree-seq-interleave-stream
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
     (tree-seq-interleave-stream #(not (is-leaf %)) get-children root)
     )
)

(defn tree-seq-interleave-stream-seq
  "Converts to sequence, makes it more greedy."
   ([branch? get-children root]
     (stream-to-seq (tree-seq-interleave-stream branch? get-children root)))
   ([root]
     (stream-to-seq (tree-seq-interleave-stream root)))
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

(defn tree-map-node
  "Creates new tree with identical structure with each node mapped to a new node.
transform node can do post creation processing on new node
transform-node [position original-node original-parent-node new-parent-node new-node]
This code could be made simpler if node kept its parent, but that would prevent nodes
from being gc-ed.
"
  ([transform-node root]
  (letfn [
          (node-is-leaf-x 
            [original-node node] 
            (is-leaf original-node))
          (node-get-children-x
            [original-parent-node new-parent-node] 
            (map 
              (fn [original-node position]
                (transform-node
                  position 
                  original-node 
                  original-parent-node 
                  new-parent-node
                  (TreeNodeX. 
                    (node-get-value original-node) 
                    (fn [n] (node-is-leaf-x original-node n)) 
                    (fn [n] (node-get-children-x original-node n)))
                  ))
              (node-get-children original-parent-node)
              (iterate inc 1)))
          ]
         (transform-node
           1 
           root 
           nil 
           nil
           (TreeNodeX. 
             (node-get-value root) 
             (fn [n] (node-is-leaf-x root n)) 
             (fn [n] (node-get-children-x root n))))))
  )

(defn tree-map-value
  "Creates new tree with identical structure with each node value mapped."
  [proc root]
  (tree-map-node 
    (fn [position original-node original-parent-node new-parent-node new-node] 
      (assoc new-node :value (proc (node-get-value new-node)))) 
    root)
  )

(defn tree-map-id
  "Creates new tree with identical structure with each node getting id vector."
  [root]
  (tree-map-node 
    (fn [position original-node original-parent-node new-parent-node new-node]
      (let [id (if new-parent-node
                 (let [parent-id (:id new-parent-node)]
                   (conj parent-id position))
                 [1])]
        (assoc new-node :id id)))
    root)
  )

(defn tree-map-value-id
  "Creates new tree with identical structure with each node getting id vector."
  [root]
  (tree-map-node 
    (fn [position original-node original-parent-node new-parent-node new-node]
      (let [value (node-get-value original-node)
            new-value (if new-parent-node
                        (let [parent-value-map (node-get-value new-parent-node)
                              parent-id (:id parent-value-map)
                              id (conj parent-id position)]
                          {:id id :value value})
                        {:id [1] :value value})]
        (assoc new-node :value new-value)))
    root)
  )

(defn seq-tree
  "Creates tree from sequence."
  [s]
  (letfn [
          (node-is-leaf 
            [sx node]
            (is-leaf sx))
          (node-get-children 
            [sx node] 
            (map 
              (fn [sxx] (TreeNodeX. sxx (fn [n] (node-is-leaf sxx n)) (fn [n] (node-get-children sxx n)))) 
              (get-children sx)))] 
         (TreeNodeX. s (fn [n] (node-is-leaf s n)) (fn [n] (node-get-children s n))))
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

(defn tree-seq-multi-depth-2
  "Walks two trees in lockstep."
   ([root1 root2]
     (let [root1x (if true (seq-tree root1) root1)
           root2x (if true (seq-tree root2) root2)]
       (tree-seq-multi-depth-2 
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
             (tree-seq-multi-depth-2 
               (rest s1x) 
               (rest s2x) 
               n1x
               n2x))))))
   )
