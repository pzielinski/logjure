(ns logjure.utils.treeimpl-test
    (:use 
    logjure.utils.treenode
    logjure.utils.treeimpl
    logjure.utils.treeseq
    logjure.utils.testing 
    clojure.contrib.test-is
    )
  (:import 
    logjure.utils.treeimpl.TreeNodeImpl
    )
)

(deftest test-seq-tree
    (is (= '() (node-get-value (seq-tree '() is-leaf get-children))))
    (is (= :a (node-get-value (seq-tree :a is-leaf get-children))))
    (is (= '(:a) 
           (map node-get-value (tree-seq-depth (seq-tree :a is-leaf get-children)))))
    (is (= '((1 2 3) 1 2 3) 
           (map node-get-value (tree-seq-depth (seq-tree '(1 2 3) is-leaf get-children)))))
    (is (= '((1 (2) 3) 1 (2) 2 3) 
           (map node-get-value (tree-seq-depth (seq-tree '(1 (2) 3) is-leaf get-children)))))
  )

(run-tests)
