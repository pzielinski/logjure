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

(deftest test-create-infinite-tree
  (let [s (tree-stream-interleave-seq (create-infinite-tree))]
    (is (= [1] (node-get-value (nth s 0))))
    (is (= [1 1] (node-get-value (nth s 1))))
    (is (= [1 1 1] (node-get-value (nth s 2))))
    (is (= [1 2] (node-get-value (nth s 3))))
    (is (= [1 1 1 1] (node-get-value (nth s 4))))
    (is (= [1 3] (node-get-value (nth s 5))))
    (is (= [1 2 1] (node-get-value (nth s 6))))
    (is (= [1 4] (node-get-value (nth s 7))))
    (is (= [1 1 1 1 1] (node-get-value (nth s 8))))
    (is (= [1 5] (node-get-value (nth s 9))))
    (is (= [1 1 2] (node-get-value (nth s 10))))
    (is (= [1 1 1 1 1 157] (node-get-value (nth s 10000))))
    (is (= [1 5001] (node-get-value (nth s 10001))))
    (is (= [1 1 1251] (node-get-value (nth s 10002))))
    (is (= [1 5002] (node-get-value (nth s 10003))))
    (is (= [1 1 1 626] (node-get-value (nth s 10004))))
    (is (= [1 5003] (node-get-value (nth s 10005))))
    (is (= [1 2 626] (node-get-value (nth s 10006))))
    (is (= [1 5004] (node-get-value (nth s 10007))))
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

(deftest test-tree-map
  (let [s (tree-stream-interleave-seq (tree-map identity (create-infinite-tree)))]
    (is (= [1] (node-get-value (nth s 0))))
    (is (= [1 1] (node-get-value (nth s 1))))
    (is (= [1 1 1] (node-get-value (nth s 2))))
    (is (= [1 2] (node-get-value (nth s 3))))
    (is (= [1 1 1 1] (node-get-value (nth s 4))))
    (is (= [1 3] (node-get-value (nth s 5))))
    (is (= [1 2 1] (node-get-value (nth s 6))))
    (is (= [1 4] (node-get-value (nth s 7))))
    (is (= [1 1 1 1 1] (node-get-value (nth s 8))))
    (is (= [1 5] (node-get-value (nth s 9))))
    (is (= [1 1 2] (node-get-value (nth s 10))))
    (is (= [1 1 1 1 1 157] (node-get-value (nth s 10000))))
    (is (= [1 5001] (node-get-value (nth s 10001))))
    (is (= [1 1 1251] (node-get-value (nth s 10002))))
    (is (= [1 5002] (node-get-value (nth s 10003))))
    (is (= [1 1 1 626] (node-get-value (nth s 10004))))
    (is (= [1 5003] (node-get-value (nth s 10005))))
    (is (= [1 2 626] (node-get-value (nth s 10006))))
    (is (= [1 5004] (node-get-value (nth s 10007))))
    )
  )

(run-tests)
