(ns logjure.utils.treeimpl
  (:use 
    logjure.utils.treenode 
    )
  )

(defrecord TreeNodeImpl [value get-children-value-and-fn-pairs])

(extend-type TreeNodeImpl
  TreeNode
  (node-is-leaf 
    [node] 
    (empty? ((:get-children-value-and-fn-pairs node) node)))
  (node-get-children 
    [node] 
    (map 
      (fn [[child-value child-get-children-value-and-fn-pairs]] 
        (TreeNodeImpl. child-value child-get-children-value-and-fn-pairs)) 
      ((:get-children-value-and-fn-pairs node) node)))
  (node-get-value 
    [node] 
    (:value node))
  )

(defn create-tree
  "Creates new tree.
get-children-values [parent-value] drives creation of the children nodes."
  [root-value get-children-values]
  (letfn 
    [(get-children-value-and-fn-pairs-x 
       [node] 
       (map (fn [value] [value get-children-value-and-fn-pairs-x]) (get-children-values (node-get-value node))))]  
    (TreeNodeImpl. root-value get-children-value-and-fn-pairs-x))
  )

(defn create-infinite-tree
  "Creates infinite tree with each node having infinite children."
  []
  (create-tree
    [1]
    (fn [parent-value] 
      (map 
        (fn [n] (conj parent-value n)) 
        (iterate inc 1)))) 
  )

(defn seq-tree
  "Creates tree from sequence."
  [the-seq is-leaf get-children]
  (create-tree
    the-seq
    (fn [s] 
      (get-children s)))
  ) 

(defn tree-map
  "Creates new tree with identical structure with each node mapped to a new node.
mapping-fn [node-value]"
  [mapping-fn root]
  (letfn 
    [(get-children-value-and-fn-pairs-x 
       [original-parent-node new-parent-node] 
       (map 
         (fn [original-node] 
           [(mapping-fn (node-get-value original-node)) (fn [n] (get-children-value-and-fn-pairs-x original-node n))]) 
         (node-get-children original-parent-node)))]  
    (TreeNodeImpl. (mapping-fn (node-get-value root)) (fn [n] (get-children-value-and-fn-pairs-x root n))))
  )

