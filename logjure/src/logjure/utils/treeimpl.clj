(ns logjure.utils.treeimpl
  (:use 
    logjure.utils.treenode 
    )
  )

(defrecord TreeNodeImpl [value get-children-values])

(extend-type TreeNodeImpl
  TreeNode
  (node-is-leaf 
    [node] 
    (empty? ((:get-children-values node) node)))
  (node-get-children 
    [node] 
    (map 
      (fn [child-value] (TreeNodeImpl. child-value (:get-children-values node))) 
      ((:get-children-values node) node)))
  (node-get-value 
    [node] 
    (:value node))
  )

(defn create-tree
  ""
  [root-value get-children-values]
  (letfn 
    [(get-children-values-x 
       [node] 
       (get-children-values (node-get-value node)))]  
    (TreeNodeImpl. root-value get-children-values-x))
  )

(defn create-tree-infinite
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

