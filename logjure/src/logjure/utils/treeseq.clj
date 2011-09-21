(ns utils.treeseq
  (:use utils.defmultimethod clojure.contrib.test-is)
  (:require clojure.inspector))

(defprotocol TreeNode
  (node-is-leaf [node])
  (node-get-children [node])
  (node-get-value [node])
  )

(defrecord TreeNodeFixed [value children])

(extend-type TreeNodeFixed
  TreeNode
  (node-is-leaf [node] (nil? (node-get-children node)))
  (node-get-children [node] (:children node))
  (node-get-value [node] (:value node))
)

(defn finalize-tree-node-dynamic-do-nothing [tree-node-dynamic])

(defn finalize-tree-node-dynamic-do-println [tree-node-dynamic]
  (println (java.util.Date.) " TreeNodeDynamic finalize " (.child-value-seq tree-node-dynamic))
  )

(def *finalize-tree-node-dynamic-fn* (atom finalize-tree-node-dynamic-do-nothing))

(defn finalize-tree-node-dynamic [tree-node-dynamic]
  (finalize-tree-node-dynamic-do-nothing tree-node-dynamic))

(deftype TreeNodeDynamic [child-value-seq]
  Cloneable
  (finalize 
    [this] 
    (@*finalize-tree-node-dynamic-fn* this))
  )

(defn create-tree-node-dynamic [child-value-seq]
  (TreeNodeDynamic. child-value-seq)
  )

(defn create-tree-node-dynamic-println [child-value-seq]
  (do
    (println (java.util.Date.) " TreeNodeDynamic create.. " child-value-seq)
    (TreeNodeDynamic. child-value-seq)
    )
  )

(defn create-tree-node-dynamic-sleep-println [child-value-seq]
  (do
    (Thread/sleep 2000)
    (println (java.util.Date.) " TreeNodeDynamic create.. " child-value-seq)
    (TreeNodeDynamic. child-value-seq)
    )
  )

(defn sleep-before [time-ms]
  (fn [f & args] (do (Thread/sleep 2000) (apply f args)))
  )

(extend-type TreeNodeDynamic 
  TreeNode
  (node-is-leaf 
    [node] 
    (let [value (node-get-value node)]
          (or (nil? value) (not (sequential? value)))))
  (node-get-children 
    [node]
    (if (node-is-leaf node)
      ()
      (let [value (node-get-value node)]
        (map #(create-tree-node-dynamic %) value))))
  (node-get-value 
    [node] 
    (.child-value-seq node))
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

(defn get-child-seq-log [node] 
  (do (println (str "get-child-seq " node)) (get-child-seq node)))

(defn get-child-seq-log-each-child [node] 
  (get-child-seq node 0 clojure.inspector/is-leaf clojure.inspector/get-child-count 
    (fn [n i] 
      (do (println)(println (str "get-child " node " index " i )) 
        (clojure.inspector/get-child n i)))))

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

(defmultimethod is-leaf [node] #(satisfies? TreeNode %)
  true (node-is-leaf node)
  :default (clojure.inspector/is-leaf node)
  )

(defmultimethod get-children [node] #(satisfies? TreeNode %)
  true (node-get-children node)
  :default (get-child-seq node)
  )

(defn deeply-nested [n]
  (loop [n n result '(:bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))


(defn tree-seq-depth
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
   [root]
   (tree-seq #(not (is-leaf %)) get-children root))

(deftest test-tree-seq-depth
  (is (= '(:a) (doall (tree-seq-depth :a))))
  (is (= '(()) (doall (tree-seq-depth '()))))
  (is (= '((:a) :a) (doall (tree-seq-depth '(:a)))))
  (is (= '((:a :b :c) :a :b :c) (doall (tree-seq-depth '(:a :b :c)))))
  (is (= '((:a (:b) :c) :a (:b) :b :c) (doall (tree-seq-depth '(:a (:b) :c)))))
  (is (= '((:a (:b (:x)) :c) :a (:b (:x)) :b (:x) :x :c) (doall (tree-seq-depth '(:a (:b (:x)) :c)))))
  (is (= '(:a :b :x :c) (doall (filter is-leaf (tree-seq-depth '(:a (:b (:x)) :c))))))
  )

(defn lazy-list-merge
  ([] (lazy-seq nil))
  ([col cols]
    (lazy-seq
      (let [s (seq col)]
        (if s
          (cons (first s) (lazy-list-merge (rest s) cols))
          (if (not (empty? cols))
            (lazy-list-merge (first cols) (rest cols)))))))
  ([cols]
       (lazy-list-merge (first cols) (rest cols)))
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

(defn get-nodes-at-depth-series
  "Returns a lazy sequence of lazy sequences. 
   Eeach child sequence contains all nodes at corresponding depth."
   ([branch? children root]
     (let [walk 
           (fn walk [branch? children parent-nodes]
             (lazy-seq
               (let [nodes (lazy-list-merge (map #(children %) parent-nodes))]
                 (when (not (empty? nodes))
                   (cons nodes
                         (walk branch? children nodes))))))]
       (lazy-seq
         (let [root-list (list root)]
           (cons root-list 
                 (walk branch? children root-list)))))
     )
   ([root] 
     (get-nodes-at-depth-series #(not (is-leaf %)) get-children root)
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

;stops walking at allowed depth
(defn get-nodes-at-depth
   ([branch? children root allowed-depth]
   (let [walk 
         (fn walk [node depth]
           (lazy-seq
             (if (< depth allowed-depth)
               (when (branch? node)
                 (lazy-list-merge (map #(walk % (inc depth)) (children node))))
               (list node))
             ))]
     (walk root 0)))
   ([root allowed-depth] 
     (get-nodes-at-depth #(not (is-leaf %)) get-children root allowed-depth))
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

(defn tree-seq-breadth-by-dive
  "Returns a lazy sequence of the nodes in a tree, via a BREADTH-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
   tree.
   This is a slower version of tree-seq-breadth, 
   but it does not need to keep all parent nodes to process children.
   It calculates nodes for each level from scratch.
   Can keep all nodes in mem up to max depth.
   In last search all nodes are kept in mem!!!!!!!!!!!!!!
   "
   ([branch? children root allowed-depth]
     (lazy-seq
      (let [nodes (get-nodes-at-depth branch? children root allowed-depth)]
        (when (not (empty? nodes))
          (cons 
            nodes
            ;(when (some #(not (is-leaf %)) nodes) ;ELIMINATED LAST DIVE IF ONLY LEAF PARENT NODES
              (tree-seq-breadth-by-dive branch? children root (inc allowed-depth))
              ;)
            ))))
       )
   ([branch? children root]
     (lazy-list-merge (tree-seq-breadth-by-dive branch? children root 0)))
   ([root]
     (tree-seq-breadth-by-dive #(not (is-leaf %)) get-children root))
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
            ;(println " RETURN " value) 
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
;(perform-test-finalize-tree-seq-breadth)

(run-tests)
