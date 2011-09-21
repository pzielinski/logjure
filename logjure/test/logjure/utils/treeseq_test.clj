(ns logjure.utils.treeseq-test
  (:use logjure.utils.treeseq clojure.contrib.test-is)
  (:import logjure.utils.treeseq.TreeNodeFixed logjure.utils.treeseq.TreeNodeDynamic)
  )

(deftest test-get-child-seq
  (is (= '() (get-child-seq :a)))
  (is (= '() (get-child-seq '())))
  (is (= '() (get-child-seq '[])))
  (is (= '(:a) (get-child-seq '(:a))))
  (is (= '(:a) (get-child-seq '[:a])))
  (is (= '(:a :b :c) (get-child-seq '(:a :b :c))))
  (is (= '(:a :b :c) (get-child-seq '[:a :b :c])))
  (is (= '(:a :b 1 () (:c :d) []) (get-child-seq '(:a :b 1 () (:c :d) []))))
  )

(deftest test-tree-seq-depth
  (is (= '(:a) (doall (tree-seq-depth :a))))
  (is (= '(()) (doall (tree-seq-depth '()))))
  (is (= '((:a) :a) (doall (tree-seq-depth '(:a)))))
  (is (= '((:a :b :c) :a :b :c) (doall (tree-seq-depth '(:a :b :c)))))
  (is (= '((:a (:b) :c) :a (:b) :b :c) (doall (tree-seq-depth '(:a (:b) :c)))))
  (is (= '((:a (:b (:x)) :c) :a (:b (:x)) :b (:x) :x :c) (doall (tree-seq-depth '(:a (:b (:x)) :c)))))
  (is (= '(:a :b :x :c) (doall (filter is-leaf (tree-seq-depth '(:a (:b (:x)) :c))))))
  )

(deftest test-lazy-list-merge
  (is (= '() (doall (lazy-list-merge))))
  (is (= '() (doall (lazy-list-merge '()))))
  (is (= '(:a) (doall (lazy-list-merge '((:a))))))
  (is (= '(:a :b) (doall (lazy-list-merge '((:a) (:b))))))
  (is (= '(:a :b (:c (:x)) :d) (doall (lazy-list-merge '((:a) (:b (:c (:x))) (:d))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-list-merge '(((:a :b (:c))) (:a :b (:c)) (:c))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-list-merge '(((:a :b (:c))) (:a :b (:c)) (:c) ())))))
  (is (= '(:a) (doall (lazy-list-merge '(() (:a))))))
  (is (= '(:a) (doall (lazy-list-merge '(() () (:a))))))
)

(defn perform-lazy-test-tree-seq-breadth-all [root]
  (let [records (atom [])
        record-fn (fn [record] (swap! records conj record))
        get-child-seq-recording (fn [node] (do (record-fn node)) (get-child-seq node))] 
    [(doall (tree-seq-breadth #(not (is-leaf %)) get-child-seq-recording root))
     @records]
    )
  )

(defn perform-lazy-test-tree-seq-breadth-nth [root n]
  (let [records (atom [])
        record-fn (fn [record] (swap! records conj record))
        get-child-seq-recording (fn [node] (do (record-fn node)) (get-child-seq node))] 
    [(nth (tree-seq-breadth #(not (is-leaf %)) get-child-seq-recording root) n :not-found)
     @records]
    )
  )

;CHECK WHY THE GET-CHILD-SEQ IS CALLED ON NODES THAT ARE NOT BRANCH NODES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn test-tree-seq-breadth-x []
  (is (= '(:a) (doall (tree-seq-breadth :a))))
  (is (= '(()) (doall (tree-seq-breadth '()))))
  (is (= '((:a) :a) (doall (tree-seq-breadth '(:a)))))
  (is (= '((:a :b :c) :a :b :c) (doall (tree-seq-breadth '(:a :b :c)))))
  (is (= '((:a (:b) :c) :a (:b) :c :b) (doall (tree-seq-breadth '(:a (:b) :c)))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (tree-seq-breadth '(:a :b (:c))))))
  (is (= '((:a (:b (:x)) :c) :a (:b (:x)) :c :b (:x) :x) (doall (tree-seq-breadth '(:a (:b (:x)) :c)))))
  (is (= '((:a ((:x) :b) :c) :a ((:x) :b) :c (:x) :b :x) (doall (tree-seq-breadth '(:a ((:x) :b) :c)))))
  (is (= '((:a ((:x) :b) ((:y) :c) :d) :a ((:x) :b) ((:y) :c) :d (:x) :b (:y) :c :x :y) 
         (doall (tree-seq-breadth '(:a ((:x) :b) ((:y) :c) :d)))))
  (is (= '(:a :c :b :x) (doall (filter is-leaf (tree-seq-breadth '(:a ((:x) :b) :c))))))
  (is (= '(
            (:a ((:x) :b) :c ((:y) :d) :e)
            :a ((:x) :b) :c ((:y) :d) :e
            (:x) :b (:y) :d
            :x :y
            ) 
         (doall (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)))))
  (is (= '(:a :c :e :b :d :x :y) (doall (filter is-leaf (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e))))))
  )

(deftest test-tree-seq-breadth
  (test-tree-seq-breadth-x)
  ;test that no stack overflow
  (is (= '(:bottom) (doall (filter is-leaf (tree-seq-breadth (deeply-nested 1000))))))
  ;test laziness
  ;level 0 (root)
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) []] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 0)))
  ;level 1
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 1)))
  (is (= '[((:x) :b) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 2)))
  (is (= '[:c 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 3)))
  (is (= '[((:y) :d) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 4)))
  (is (= '[:e [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 5)))
  ;level 2
  (is (= '[(:x) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 6)))
  (is (= '[:b 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 7)))
  (is (= '[(:y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 8)))
  (is (= '[:d 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 9)))
  ;level 3
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 10)))
  (is (= '[:y 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 11)))
  ;not found
  (is (= '[:not-found 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y) :d :x :y]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 12)))
  ;all
  (is (= '[(
            (:a ((:x) :b) :c ((:y) :d) :e)
            :a ((:x) :b) :c ((:y) :d) :e
            (:x) :b (:y) :d
            :x :y
            ) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y) :d :x :y]] 
         (perform-lazy-test-tree-seq-breadth-all '(:a ((:x) :b) :c ((:y) :d) :e))))
  )

(deftest test-get-nodes-at-depth
  (is (= '(:a) (doall (get-nodes-at-depth :a 0))))
  (is (= '() (doall (get-nodes-at-depth :a 1))))
  
  (is (= '((:a)) (doall (get-nodes-at-depth '(:a) 0))))
  (is (= '(:a) (doall (get-nodes-at-depth '(:a) 1))))
  
  (is (= '((:a :b :c)) (doall (get-nodes-at-depth '(:a :b :c) 0))))
  (is (= '(:a :b :c) (doall (get-nodes-at-depth '(:a :b :c) 1))))
  
  (is (= '((:a (:b) :c)) (doall (get-nodes-at-depth '(:a (:b) :c) 0))))
  (is (= '(:a (:b) :c) (doall (get-nodes-at-depth '(:a (:b) :c) 1))))
  (is (= '(:b) (doall (get-nodes-at-depth '(:a (:b) :c) 2))))
  (is (= '() (doall (get-nodes-at-depth '(:a (:b) :c) 3))))
  
  (is (= '((:a ((:x) :b) :c ((:y) :d) :e)) (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 0))))
  (is (= '(:a ((:x) :b) :c ((:y) :d) :e) (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 1))))
  (is (= '((:x) :b (:y) :d) (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2))))
  (is (= '(:x :y) (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 3))))
  (is (= '() (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 4))))

  (is (= '((:bottom)) (get-nodes-at-depth (deeply-nested 100) 100)))
)

(deftest test-tree-seq-breadth-by-dive
  (binding [tree-seq-breadth tree-seq-breadth-by-dive]
    (test-tree-seq-breadth-x)
  ;test that no stack overflow - slow for large values
  (is (= '(:bottom) (doall (filter is-leaf (tree-seq-breadth (deeply-nested 100))))))
  ;test laziness
  ;level 0 (root)
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) []] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 0)))
  ;level 1
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 1)))
  (is (= '[((:x) :b) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 2)))
  (is (= '[:c 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 3)))
  (is (= '[((:y) :d) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 4)))
  (is (= '[:e [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 5)))
  ;level 2
  (is (= '[(:x) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 6)))
  (is (= '[:b 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 7)))
  (is (= '[(:y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 8)))
  (is (= '[:d 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 9)))
  ;level 3
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 10)))
  (is (= '[:y 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 11)))
  ;not found
  (is (= '[:not-found 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (perform-lazy-test-tree-seq-breadth-nth '(:a ((:x) :b) :c ((:y) :d) :e) 12)))
  ;all
  (is (= '[(
            (:a ((:x) :b) :c ((:y) :d) :e)
            :a ((:x) :b) :c ((:y) :d) :e
            (:x) :b (:y) :d
            :x :y
            ) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y) 
            (:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (perform-lazy-test-tree-seq-breadth-all '(:a ((:x) :b) :c ((:y) :d) :e))))
    ))

(deftest test-tree-seq-with-treenode-fixed
  (let [
        tn031 (TreeNodeFixed. :tn031 nil)
        tn012 (TreeNodeFixed. :tn012 nil)
        tn011 (TreeNodeFixed. :tn011 nil)
        tn03  (TreeNodeFixed. :tn03  [tn031])
        tn02  (TreeNodeFixed. :tn02  nil)
        tn01  (TreeNodeFixed. :tn01  [tn011 tn012])
        tn0   (TreeNodeFixed. :tn0   [tn01 tn02 tn03])
        sample-tree (TreeNodeFixed. :root 
                            (list (TreeNodeFixed.  :a1 nil)
                                  (TreeNodeFixed.  :a2 nil)
                                  (TreeNodeFixed.  :a3 
                                                   (list (TreeNodeFixed. :a3_1 nil)))))
        ]
    (is (= false (is-leaf tn0)))
    (is (= [tn01 tn02 tn03] (get-children tn0)))
    (is (= [tn0 tn01 tn011 tn012 tn02 tn03 tn031] (tree-seq-depth tn0)))
    (is (= [tn0 tn01 tn02 tn03 tn011 tn012 tn031] (tree-seq-breadth tn0)))
    (is (= (tree-seq-breadth tn0) (tree-seq-breadth-by-dive tn0)))
    (is (= '(:root :a1 :a2 :a3 :a3_1) (map #(node-get-value %) (tree-seq-breadth-by-dive sample-tree))))
   )
  )

(deftest test-tree-seq-with-treenode-dynamic
    (is (= '(:a ((:x) :b) :c ((:y) :d) :e) 
           (map #(node-get-value %) (get-children (create-tree-node-dynamic '(:a ((:x) :b) :c ((:y) :d) :e))))))
  )

(defn node-mapping-fn [node]
  (node-get-value node))

(defn node-mapping-fn-gc-sleep [node]
  (let [value (node-get-value node)] 
    (println (java.util.Date.) " RETURN " value) 
    (System/gc) 
    (Thread/sleep 2000) 
    value)
  )

(defn test-tree-seq-with-treenode-dynamic-finalization-1 []
    (is (= '((:a ((:x) :b) :c ((:y) :d) :e) 
              :a ((:x) :b) (:x) :x :b :c ((:y) :d) (:y) :y :d :e
              ) 
           (map node-mapping-fn 
                (tree-seq-depth (create-tree-node-dynamic '(:a ((:x) :b) :c ((:y) :d) :e))))))
  (try
    (println "finalization test waiting ...")
    (Thread/sleep 60000)
    (println "finalization test start")
    (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-do-println)
    (binding [create-tree-node-dynamic create-tree-node-dynamic-println 
            ;finalize-tree-node-dynamic-do-nothing finalize-tree-node-dynamic-do-println
            node-mapping-fn node-mapping-fn-gc-sleep]
    (is (= '((:a ((:x) :b) :c ((:y) :d) :e) 
              :a ((:x) :b) :c ((:y) :d) :e 
              (:x) :b (:y) :d 
              :x :y
              ) 
           (map node-mapping-fn 
                (tree-seq-breadth (create-tree-node-dynamic '(:a ((:x) :b) :c ((:y) :d) :e))))))
    (System/gc)
    (Thread/sleep 5000)
    )
    (finally (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-do-nothing)))
  )

(defn test-tree-seq-with-treenode-dynamic-finalization-2 []
  (try
    (println "finalization test start")
    (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-do-println)
    (binding [create-tree-node-dynamic create-tree-node-dynamic-sleep-println 
            ;finalize-tree-node-dynamic-do-nothing finalize-tree-node-dynamic-do-println
            node-mapping-fn node-mapping-fn-gc-sleep]
           (doall (map node-mapping-fn 
                (
                  ;tree-seq-depth
                  ;tree-seq-breadth 
                  ;tree-seq-breadth-by-dive
                  get-nodes-at-depth
                  (create-tree-node-dynamic '(:a ((:x) :b) :c ((:y (:yy)) :d) :e))
                  4
                  ))))
    (System/gc)
    (Thread/sleep 5000)
    (println "finalization test end")
    (finally (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-do-nothing)))
  )

(defn perform-test-finalize-tree-seq-x [tree-seq-fn root-child-value-seq]
  (let [records (atom [])
        sleep2000 (sleep-before 2000)
        record-fn 
        (fn record-fn [record] 
          (swap! records conj record))
        finalize-tree-node-dynamic-record 
        (fn finalize-tree-node-dynamic-record [node] 
          ;(println "finalize " (node-get-value node))
          (record-fn {:final_ (node-get-value node)}))
        create-tree-node-dynamic-sleep-record 
        (fn create-tree-node-dynamic-sleep-record 
          [child-value-seq] 
          (Thread/sleep 2000)
          ;(println "create " child-value-seq)
          (record-fn {:create child-value-seq})
          (TreeNodeDynamic. child-value-seq))
        node-mapping-fn-gc-sleep
        (fn node-mapping-fn-gc-sleep 
          [node]
          (let [value (node-get-value node)] 
            ;(println (java.util.Date.) " RETURN " value) 
            (println " RETURN " value) 
            (record-fn {:RETURN value})
            (System/gc) 
            (Thread/sleep 2000) 
            value))
        ] 
    (try
      (println "perform-test-finalize-tree-seq-x " tree-seq-fn root-child-value-seq)
      (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-record)
      (binding [create-tree-node-dynamic create-tree-node-dynamic-sleep-record 
                node-mapping-fn node-mapping-fn-gc-sleep]
        (let [result 
              (doall 
                (map node-mapping-fn 
                     (tree-seq-fn 
                       (create-tree-node-dynamic root-child-value-seq))))
              ]
          (System/gc)
          (Thread/sleep 5000)
          (println "perform-test-finalize-tree-seq-x done")
          {:result result :records @records}
        ))
      (finally (reset! *finalize-tree-node-dynamic-fn* finalize-tree-node-dynamic-do-nothing)))
    )
  )

(defn perform-test-finalize-tree-seq-breadth [] 
  (let [{:keys [result records] :as result-records-map} 
        (perform-test-finalize-tree-seq-x tree-seq-breadth '(:a ((:x) :b) :c ((:y (:yy)) :d) :e))]
    (is (= '((:a ((:x) :b) :c ((:y (:yy)) :d) :e) 
              :a ((:x) :b) :c ((:y (:yy)) :d) :e 
              (:x) :b (:y (:yy)) :d 
              :x :y (:yy) 
              :yy)
           result))
    (is (= '[{:create (:a ((:x) :b) :c ((:y (:yy)) :d) :e)} 
             {:RETURN (:a ((:x) :b) :c ((:y (:yy)) :d) :e)} 
             {:create :a} 
             {:RETURN :a} 
             {:final_ (:a ((:x) :b) :c ((:y (:yy)) :d) :e)} 
             {:create ((:x) :b)} 
             {:RETURN ((:x) :b)} 
             {:create :c} 
             {:RETURN :c} 
             {:create ((:y (:yy)) :d)} 
             {:RETURN ((:y (:yy)) :d)} 
             {:create :e} 
             {:RETURN :e} 
             {:create (:x)} 
             {:RETURN (:x)} 
             {:final_ ((:x) :b)} 
             {:final_ :a} 
             {:create :b} 
             {:RETURN :b} 
             {:create (:y (:yy))} 
             {:RETURN (:y (:yy))} 
             {:final_ ((:y (:yy)) :d)} 
             {:final_ :c} 
             {:create :d} 
             {:RETURN :d} 
             {:create :x} 
             {:RETURN :x} 
             {:final_ (:x)} 
             {:final_ :e} 
             {:create :y} 
             {:RETURN :y} 
             {:final_ (:y (:yy))} 
             {:final_ :b} 
             {:create (:yy)} 
             {:RETURN (:yy)} 
             {:create :yy} 
             {:RETURN :yy} 
             {:final_ (:yy)} 
             {:final_ :y} 
             {:final_ :x} 
             {:final_ :d} 
             {:final_ :yy}]
           records))
        ;result-records-map
        ))


(deftest test-finalize-tree-seq-breadth
  ;(perform-test-finalize-tree-seq-breadth)
)

(run-tests)
