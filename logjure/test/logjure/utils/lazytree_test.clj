(ns logjure.utils.lazytree-test
  (:use 
    logjure.utils.lazytree
    logjure.utils.treeseq
    logjure.utils.testing 
    clojure.contrib.test-is
    )
  (:import 
    logjure.utils.lazytree.TreeNodeX
    )
  )

(deftest test-seq-tree
    (is (= {:id [1] :value '()} (node-get-value (tree-map-value-id (seq-tree '() is-leaf get-children)))))
    (is (= {:id [1] :value :a} (node-get-value (tree-map-value-id (seq-tree :a is-leaf get-children)))))
    (is (= '({:id [1] :value :a}) 
           (doall (map node-get-value (tree-seq-depth (tree-map-value-id (seq-tree :a is-leaf get-children)))))))
    (is (= '( {:id [1] :value (1 2 3)}
              {:id [1 1] :value 1}
              {:id [1 2] :value 2}
              {:id [1 3] :value 3}
              ) 
           (doall (map node-get-value (tree-seq-depth (tree-map-value-id (seq-tree '(1 2 3) is-leaf get-children)))))))
    (is (= '( {:id [1] :value (1 (2) 3)}
              {:id [1 1] :value 1}
              {:id [1 2] :value (2)}
              {:id [1 2 1] :value 2}
              {:id [1 3] :value 3}
              ) 
           (doall (map node-get-value (tree-seq-depth (tree-map-value-id (seq-tree '(1 (2) 3) is-leaf get-children)))))))
  )

(deftest test-tree-map-value
  (is (= [1] (node-get-value (tree-map-value identity (TreeNodeX. [1] is-leaf get-children)))))
  (is (= [1] (node-get-value (tree-map-value (fn [value] value) (TreeNodeX. [1] is-leaf get-children)))))
  (is (= [1 :a] (node-get-value (tree-map-value (fn [value] (conj value :a)) (TreeNodeX. [1] is-leaf get-children)))))
  (let [tree (create-fixed-node 
               [1] 
               (list (create-fixed-node  [1 1] nil)
                     (create-fixed-node  [1 2] nil)
                     (create-fixed-node  [1 3] (list (create-fixed-node [1 3 1] nil)))))]
    ;root
    (is (= [1] (node-get-value (tree-map-value identity tree))))
    ;children
    (is (= [1 1] (node-get-value (first (node-get-children (tree-map-value identity tree))))))
    (is (= [1 2] (node-get-value (second (node-get-children (tree-map-value identity tree))))))
    (is (= [1 3] (node-get-value (nth (node-get-children (tree-map-value identity tree)) 2))))
    (is (= :not-found (nth (node-get-children (tree-map-value identity tree)) 3 :not-found)))
    ;sequence
    (is (= [1] (node-get-value (nth (tree-seq-depth (tree-map-value identity tree)) 0))))
    (is (= [1 1] (node-get-value (nth (tree-seq-depth (tree-map-value identity tree)) 1))))
    (is (= [1 2] (node-get-value (nth (tree-seq-depth (tree-map-value identity tree)) 2))))
    (is (= [1 3] (node-get-value (nth (tree-seq-depth (tree-map-value identity tree)) 3))))
    (is (= [1 3 1] (node-get-value (nth (tree-seq-depth (tree-map-value identity tree)) 4))))
    (is (= :not-found (nth (tree-seq-depth (tree-map-value identity tree)) 5 :not-found)))
    ;mapping
    (letfn 
      [(mapping-fn 
         [value] 
         (conj value :a))]
      (is (= [1 :a] (node-get-value (nth (tree-seq-depth (tree-map-value mapping-fn tree)) 0))))
      (is (= [1 1 :a] (node-get-value (nth (tree-seq-depth (tree-map-value mapping-fn tree)) 1))))
      (is (= [1 2 :a] (node-get-value (nth (tree-seq-depth (tree-map-value mapping-fn tree)) 2))))
      (is (= [1 3 :a] (node-get-value (nth (tree-seq-depth (tree-map-value mapping-fn tree)) 3))))
      (is (= [1 3 1 :a] (node-get-value (nth (tree-seq-depth (tree-map-value mapping-fn tree)) 4)))))
    )
  ;test with fixed tree
  (let [
        tn031 (create-fixed-node :tn031 nil)
        tn012 (create-fixed-node :tn012 nil)
        tn011 (create-fixed-node :tn011 nil)
        tn03  (create-fixed-node :tn03  [tn031])
        tn02  (create-fixed-node :tn02  nil)
        tn01  (create-fixed-node :tn01  [tn011 tn012])
        tn0   (create-fixed-node :tn0   [tn01 tn02 tn03])
        mapping-fn (fn mapping-fn [value] (vector :a value))
        mapped-tree (tree-map-value mapping-fn tn0)
        node-seq (tree-seq-depth mapped-tree)
        value-seq (map node-get-value node-seq)
        ]
    (is (= [:a :tn0] (nth value-seq 0)))
    (is (= [:a :tn01] (nth value-seq 1)))
    (is (= [:a :tn011] (nth value-seq 2)))
    (is (= [:a :tn012] (nth value-seq 3)))
    (is (= [:a :tn02] (nth value-seq 4)))
    (is (= [:a :tn03] (nth value-seq 5)))
    (is (= [:a :tn031] (nth value-seq 6)))
    (is (= :not-found (nth value-seq 7 :not-found)))
   )
  ;test with infinite tree
  (let [mapping-fn (fn mapping-fn [vect] (conj vect :a))
        s (tree-stream-interleave-seq (tree-map-value mapping-fn (create-infinite-tree)))]
    (is (= [1 :a] (node-get-value (nth s 0))))
    (is (= [1 1 :a] (node-get-value (nth s 1))))
    (is (= [1 1 1 :a] (node-get-value (nth s 2))))
    (is (= [1 2 :a] (node-get-value (nth s 3))))
    (is (= [1 1 1 1 :a] (node-get-value (nth s 4))))
    (is (= [1 3 :a] (node-get-value (nth s 5))))
    (is (= [1 2 1 :a] (node-get-value (nth s 6))))
    (is (= [1 4 :a] (node-get-value (nth s 7))))
    (is (= [1 1 1 1 1 :a] (node-get-value (nth s 8))))
    (is (= [1 5 :a] (node-get-value (nth s 9))))
    (is (= [1 1 2 :a] (node-get-value (nth s 10))))
    (is (= [1 1 1 1 1 157 :a] (node-get-value (nth s 10000))))
    (is (= [1 5001 :a] (node-get-value (nth s 10001))))
    (is (= [1 1 1251 :a] (node-get-value (nth s 10002))))
    (is (= [1 5002 :a] (node-get-value (nth s 10003))))
    (is (= [1 1 1 626 :a] (node-get-value (nth s 10004))))
    (is (= [1 5003 :a] (node-get-value (nth s 10005))))
    (is (= [1 2 626 :a] (node-get-value (nth s 10006))))
    (is (= [1 5004 :a] (node-get-value (nth s 10007))))
    )
)

(deftest test-tree-map-value-id
  (let [s (tree-stream-interleave-seq (tree-map-value-id (create-infinite-tree)))]
    (is (= {:id [1] :value [1]} (node-get-value (nth s 0))))
    (is (= {:id [1 1] :value [1 1]} (node-get-value (nth s 1))))
    (is (= {:id [1 1 1] :value [1 1 1]} (node-get-value (nth s 2))))
    (is (= {:id [1 2] :value [1 2]} (node-get-value (nth s 3))))
    (is (= {:id [1 1 1 1] :value [1 1 1 1]} (node-get-value (nth s 4))))
    (is (= {:id [1 3] :value [1 3]} (node-get-value (nth s 5))))
    (is (= {:id [1 2 1] :value [1 2 1]} (node-get-value (nth s 6))))
    (is (= {:id [1 4] :value [1 4]} (node-get-value (nth s 7))))
    (is (= {:id [1 1 1 1 1] :value [1 1 1 1 1]} (node-get-value (nth s 8))))
    (is (= {:id [1 5] :value [1 5]} (node-get-value (nth s 9))))
    (is (= {:id [1 1 2] :value [1 1 2]} (node-get-value (nth s 10))))
    (is (= {:id [1 1 1 1 1 157] :value [1 1 1 1 1 157]} (node-get-value (nth s 10000))))
    (is (= {:id [1 5001] :value [1 5001]} (node-get-value (nth s 10001))))
    (is (= {:id [1 1 1251] :value [1 1 1251]} (node-get-value (nth s 10002))))
    (is (= {:id [1 5002] :value [1 5002]} (node-get-value (nth s 10003))))
    (is (= {:id [1 1 1 626] :value [1 1 1 626]} (node-get-value (nth s 10004))))
    (is (= {:id [1 5003] :value [1 5003]} (node-get-value (nth s 10005))))
    (is (= {:id [1 2 626] :value [1 2 626]} (node-get-value (nth s 10006))))
    (is (= {:id [1 5004] :value [1 5004]} (node-get-value (nth s 10007))))
    )
  )

(deftest test-tree-map-id
  (let [s (tree-stream-interleave-seq (tree-map-id (create-infinite-tree)))]
    (is (= (:id (nth s 0)) (node-get-value (nth s 0))))
    (is (= (:id (nth s 10000)) (node-get-value (nth s 10000))))
    )
  )

(run-tests)
