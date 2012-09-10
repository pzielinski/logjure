(ns logjure.utils.treenode
  (:use 
    logjure.utils.defmultimethod 
    )
  (:require clojure.inspector)
  )

(defprotocol TreeNode
  (node-is-leaf [node])
  (node-get-children [node])
  (node-get-value [node])
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

(comment
(defmultimethod get-children [node] #(satisfies? TreeNode %)
  true (node-get-children node)
  :default (get-child-seq node)
  )
)

(defmulti ^:dynamic get-children #(satisfies? TreeNode %))
(defmethod get-children true [node]
 (node-get-children node))
(defmethod get-children :default [node]
  (get-child-seq node))

