(ns logjure.utils.lazytree
  (:use 
    logjure.utils.defmultimethod 
    )
  )

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

(defn seq-tree
  "Creates tree from sequence."
  [s is-leaf get-children]
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
            (node-is-leaf original-node))
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

(defn create-infinite-tree
  "Creates infinite tree with each node having infinite children."
  []
  (letfn [
          (node-is-leaf 
            [node] 
            false)
          (node-get-children 
            [node] 
            (let [parent-vector (node-get-value node)] 
              (map 
                (fn [n] (TreeNodeX. (conj parent-vector n) node-is-leaf node-get-children)) 
                (iterate inc 1))))] 
         (TreeNodeX. [1] node-is-leaf node-get-children))
  )

(defn tree-disjoin
  "Creates new tree with structure that is overlay of argument trees, each new node value 
is sequence of argument tree node values, nil is used if no node in argument tree.
"
  ([root1 root2]
  (letfn 
    [(node-is-leaf-x 
       [node1 node2] 
       (or (and node1 (node-is-leaf node1)) (and node2 (node-is-leaf node2))))
     (node-get-children-x
       [node1 node2]
       (take-while
         (fn [n] (let [[val1 val2] (node-get-value n)] (or val1 val2)))
         (map 
           (fn 
             [n1 n2]
             (TreeNodeX. 
               (vector (if n1 (node-get-value n1) nil) (if n2 (node-get-value n2) nil))
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

