(ns logjure.utils.lazytree
  (:use 
    logjure.utils.treenode 
    )
  )

(defrecord TreeNodeX [value is-leaf get-children])

(extend-type TreeNodeX
  TreeNode
  (node-is-leaf [node] ((:is-leaf node) node))
  (node-get-children [node] ((:get-children node) node))
  (node-get-value [node] (:value node))
  )

(defn create-tree
  "Creates new tree.
get-children-values [parent-value] drives creation of the children nodes."
  [root-value get-children-values]
  (letfn [
          (node-is-leaf 
            [node] 
            (empty? (node-get-children node)))
          (node-get-children 
            [node] 
            (let [parent-value (node-get-value node)] 
              (map 
                (fn [child-value] (TreeNodeX. child-value node-is-leaf node-get-children)) 
                (get-children-values parent-value))))] 
         (TreeNodeX. root-value node-is-leaf node-get-children))
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
  ([the-seq]
  (seq-tree the-seq is-leaf get-children))
  ([the-seq is-leaf get-children]
  (create-tree
    the-seq
    (fn [s] 
      (get-children s))))
  ) 

(defn tree-map
  "Creates new tree with identical structure with each node mapped to a new node.
transform-node [new-node]"
  [transform-node root]
  (letfn 
    [(node-is-leaf-x 
       [original-node new-node] 
       (node-is-leaf original-node))
     (node-get-children-x
       [original-parent-node new-parent-node] 
       (map 
         (fn [original-node]
           (transform-node
             (TreeNodeX. 
               (node-get-value original-node) 
               (fn [n] (node-is-leaf-x original-node n)) 
               (fn [n] (node-get-children-x original-node n)))))
         (node-get-children original-parent-node)))]
    (transform-node
      (TreeNodeX. 
        (node-get-value root) 
        (fn [n] (node-is-leaf-x root n)) 
        (fn [n] (node-get-children-x root n)))))
  )

(defn tree-map-reduce
  "Creates new tree with identical structure with each node mapped to a new node.
transform-node [new-parent-node new-node] has access to both parent and node.
"
  ([transform-node root]
  (letfn [
          (node-is-leaf-x 
            [original-node new-node] 
            (node-is-leaf original-node))
          (node-get-children-x
            [original-parent-node new-parent-node] 
            (map 
              (fn [original-node]
                (transform-node
                  new-parent-node
                  (TreeNodeX. 
                    (node-get-value original-node) 
                    (fn [n] (node-is-leaf-x original-node n)) 
                    (fn [n] (node-get-children-x original-node n)))))
              (node-get-children original-parent-node)))
          ]
         (transform-node
           nil 
           (TreeNodeX. 
             (node-get-value root) 
             (fn [n] (node-is-leaf-x root n)) 
             (fn [n] (node-get-children-x root n))))))
  )

(defn tree-map-value
  "Creates new tree with identical structure with each node value mapped."
  [proc root]
  (tree-map
    (fn [new-node] 
      (assoc new-node :value (proc (node-get-value new-node)))) 
    root)
  )

(defn create-fixed-node
  [value children]
  (letfn [(node-get-children 
            [node] 
            children)
          (node-is-leaf 
            [node] 
            (nil? (node-get-children node)))] 
         (TreeNodeX. value node-is-leaf node-get-children))
  )

(defn tree-disjoin
  "Creates new tree with structure that is overlay of argument trees, 
all branches from both trees are covered. 
Each new node value is sequence of argument tree node values, nil is used if no node in argument tree.
TODO: use special symbol for no node instead of nil.
Guarantees that all returned nodes are leaves."
  ([root1 root2 not-found]
  (letfn 
    [(node-is-leaf-x 
       [node1 node2] 
       (and (or (not node1) (node-is-leaf node1)) (or (not node2) (node-is-leaf node2))))
     (node-get-children-x
       [node1 node2]
       (take-while
         (fn [n] (let [[val1 val2] (node-get-value n)] (or (not (= val1 not-found)) (not (= val2 not-found)))))
         (map 
           (fn 
             [n1 n2]
             (TreeNodeX. 
               (vector (if n1 (node-get-value n1) not-found) (if n2 (node-get-value n2) not-found))
               (fn [n] (node-is-leaf-x n1 n2)) 
               (fn [n] (node-get-children-x n1 n2)))
             )
           (if node1 
             (concat (node-get-children node1) (repeat nil)) 
             (repeat nil))
           (if node2 
             (concat (node-get-children node2) (repeat nil)) 
             (repeat nil))
           )))]
    (TreeNodeX. 
      (vector (node-get-value root1) (node-get-value root2)) 
      (fn [n] (node-is-leaf-x root1 root2)) 
      (fn [n] (node-get-children-x root1 root2)))))
  )

(defn tree-conjoin
  "Creates new tree with structure that is overlay of argument trees, 
only matching branches from both trees are covered. 
Each new node value is sequence of argument tree node values.
Some nodes may not be leaves."
  ([root1 root2]
  (letfn 
    [(node-is-leaf-x 
       [node1 node2] 
       (or (node-is-leaf node1) (node-is-leaf node2)))
     (node-get-children-x
       [node1 node2]
       (map 
         (fn 
           [n1 n2]
           (TreeNodeX. 
             (vector (node-get-value n1) (node-get-value n2))
             (fn [n] (node-is-leaf-x n1 n2)) 
             (fn [n] (node-get-children-x n1 n2)))
           )
         (node-get-children node1) 
         (node-get-children node2) 
         ))]
    (TreeNodeX. 
      (vector (node-get-value root1) (node-get-value root2)) 
      (fn [n] (node-is-leaf-x root1 root2)) 
      (fn [n] (node-get-children-x root1 root2)))))
  )

(defn tree-map-value-id
  "Creates new tree with identical structure with each node getting id vector."
  [root]
  (tree-map-value 
    (fn [[val1 val2]] {:id val1, :value val2}) 
    (tree-conjoin (create-infinite-tree) root))
  )

(defn tree-map-id
  "Creates new tree with identical structure with each node getting id vector."
  [root]
  (tree-map
    (fn [node]
      (let 
        [value (node-get-value node)
         [val1 val2] value
         id val1]
        (assoc node :id id :value val2)))
    (tree-conjoin (create-infinite-tree) root))
  )
