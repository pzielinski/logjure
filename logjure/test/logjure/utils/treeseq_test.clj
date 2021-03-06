(ns logjure.utils.treeseq-test
  (:use 
    logjure.utils.stream
    logjure.utils.treenode
    logjure.utils.lazytree
    logjure.utils.treeseq
    logjure.utils.testing 
    clojure.test
    )
  (:import 
    logjure.utils.lazytree.TreeNodeX
    )
  )

(defn finalize-tree-node-dynamic-do-nothing [tree-node-dynamic])

(def ^:dynamic *finalize-tree-node-dynamic-fn* (atom finalize-tree-node-dynamic-do-nothing))

(deftype TreeNodeDynamic [child-value-seq]
  Cloneable
  (finalize 
    [this] 
    (@*finalize-tree-node-dynamic-fn* this))
  )

(defn create-tree-node-dynamic [child-value-seq]
  (TreeNodeDynamic. child-value-seq)
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

(defn create-tree-node-dynamic-println 
  [child-value-seq]
  (do
    (println (java.util.Date.) " TreeNodeDynamic create.. " child-value-seq)
    (TreeNodeDynamic. child-value-seq)
    )
  )

(defn create-tree-node-dynamic-sleep-println 
  [child-value-seq]
  (do
    (Thread/sleep 2000)
    (println (java.util.Date.) " TreeNodeDynamic create.. " child-value-seq)
    (TreeNodeDynamic. child-value-seq)
    )
  )

(defn finalize-tree-node-dynamic 
  [tree-node-dynamic]
  (finalize-tree-node-dynamic-do-nothing tree-node-dynamic))

(defn finalize-tree-node-dynamic-do-println 
  [tree-node-dynamic]
  (println (java.util.Date.) " TreeNodeDynamic finalize " (.child-value-seq tree-node-dynamic))
  )

(defn get-child-seq-log 
  [node] 
  (do (println (str "get-child-seq " node)) (get-child-seq node)))

(defn get-child-seq-log-each-child 
  [node] 
  (get-child-seq node 0 clojure.inspector/is-leaf clojure.inspector/get-child-count 
    (fn [n i] 
      (do (println)(println (str "get-child " node " index " i )) 
        (clojure.inspector/get-child n i)))))

(defn sleep-before 
  [time-ms]
  (fn [f & args] (do (Thread/sleep 2000) (apply f args)))
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

(deftest test-deep-equal?
  (is (= false (deep-equal? (deeply-nested 10000 '?x) (deeply-nested 10000 'b))))
  (is (= true (deep-equal? (deeply-nested 10000 '?x) (deeply-nested 10000 '?x))))
)

(deftest test-tree-seq-depth
  (is (= '(:a) (doall (tree-seq-depth :a))))
  (is (= '(()) (doall (tree-seq-depth '()))))
  (is (= '((:a) :a) (doall (tree-seq-depth '(:a)))))
  (is (= '((:a :b :c) :a :b :c) (doall (tree-seq-depth '(:a :b :c)))))
  (is (= '((:a (:b) :c) :a (:b) :b :c) (doall (tree-seq-depth '(:a (:b) :c)))))
  (is (= '((:a (:b (:x)) :c) :a (:b (:x)) :b (:x) :x :c) (doall (tree-seq-depth '(:a (:b (:x)) :c)))))
  (is (= '(:a :b :x :c) (doall (filter is-leaf (tree-seq-depth '(:a (:b (:x)) :c))))))
  ;test that no stack overflow; passes 100000
  (is (= '(:bottom) (doall (filter is-leaf (tree-seq-depth (deeply-nested 10000))))))
  )

(deftest test-tree-seq-depth-lazy
  ;test laziness
  ;level 0 (root)
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 0 :not-found))))
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 1 :not-found))))
  (is (= '[((:x) :b) 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 2 :not-found))))
  (is (= '[(:x)
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 3 :not-found))))
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 4 :not-found))))
  (is (= '[:b
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 5 :not-found))))
  (is (= '[:c
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 6 :not-found))))
  (is (= '[((:y) :d)
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 7 :not-found))))
  (is (= '[(:y)
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 8 :not-found))))
  (is (= '[:y
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 9 :not-found))))
  (is (= '[:d
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 10 :not-found))))
  (is (= '[:e
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (nth (tree-seq-depth '(:a ((:x) :b) :c ((:y) :d) :e)) 11 :not-found))))
  )

  (deftest test-lazy-list-merge
  (is (= '() (doall (lazy-list-merge '()))))
  (is (= '(:a) (doall (lazy-list-merge '((:a))))))
  (is (= '(:a :b) (doall (lazy-list-merge '((:a) (:b))))))
  (is (= '(:a :b (:c (:x)) :d) (doall (lazy-list-merge '((:a) (:b (:c (:x))) (:d))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-list-merge '(((:a :b (:c))) (:a :b (:c)) (:c))))))
  (is (= '((:a :b (:c)) :a :b (:c) :c) (doall (lazy-list-merge '(((:a :b (:c))) (:a :b (:c)) (:c) ())))))
  (is (= '(:a) (doall (lazy-list-merge '(() (:a))))))
  (is (= '(:a) (doall (lazy-list-merge '(() () (:a))))))
  ;fails - because vector is stripped
  ;(is (= '([:y :y {?x :x, ?y :y}]) (doall (lazy-list-merge '() '([:y :y {?x :x, ?y :y}])))))
)

(defn ^:dynamic inc
  [x] (clojure.core/inc x))

(deftest test-lazy-list-merge-vs-apply-concat
  ;TEST LAZINESS
  ;notice that "apply concat" greedily evaluates first three items in the result seqence
  (is (= [1 [1 2 3]] (recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 0))))
  (is (= [2 [1 2 3]] (recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 1))))
  (is (= [3 [1 2 3]] (recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 2))))
  (is (= [4 [1 2 3 4]] (recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 3))))
  (is (= [5 [1 2 3 4 5]] (recorder inc #(+ % 1) (nth (apply concat (map list (iterate inc 1))) 4))))
  ;notice that "lazy-list-merge" does not
  (is (= [1 [1]] (recorder inc #(+ % 1) (nth (lazy-list-merge (map list (iterate inc 1))) 0))))
  (is (= [2 [1 2]] (recorder inc #(+ % 1) (nth (lazy-list-merge (map list (iterate inc 1))) 1))))
  (is (= [3 [1 2 3]] (recorder inc #(+ % 1) (nth (lazy-list-merge (map list (iterate inc 1))) 2))))
  (is (= [4 [1 2 3 4]] (recorder inc #(+ % 1) (nth (lazy-list-merge (map list (iterate inc 1))) 3))))
  (is (= [5 [1 2 3 4 5]] (recorder inc #(+ % 1) (nth (lazy-list-merge (map list (iterate inc 1))) 4))))
  ;"lazy-list-merge-x" is the most lazy of all, does not even call inc on current arg
  (is (= [1 []] (recorder inc #(+ % 1) (nth (lazy-list-merge-x (map list (iterate inc 1))) 0))))
  (is (= [2 [1]] (recorder inc #(+ % 1) (nth (lazy-list-merge-x (map list (iterate inc 1))) 1))))
  (is (= [3 [1 2]] (recorder inc #(+ % 1) (nth (lazy-list-merge-x (map list (iterate inc 1))) 2))))
  (is (= [4 [1 2 3]] (recorder inc #(+ % 1) (nth (lazy-list-merge-x (map list (iterate inc 1))) 3))))
  (is (= [5 [1 2 3 4]] (recorder inc #(+ % 1) (nth (lazy-list-merge-x (map list (iterate inc 1))) 4))))
  ;TEST StackOverflow
  (is (= 10001 (nth (apply concat (map list (iterate inc 1))) 10000)))
  (is (= 10001 (nth (lazy-list-merge (map list (iterate inc 1))) 10000)))
  (is (= 10001 (nth (lazy-list-merge-x (map list (iterate inc 1))) 10000)))
)

;CHECK WHY THE GET-CHILD-SEQ IS CALLED ON NODES THAT ARE NOT BRANCH NODES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn do-base-test-tree-seq-breadth 
  []
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
  (do-base-test-tree-seq-breadth)
  ;test that no stack overflow; passes 100000
  (is (= '(:bottom) (doall (filter is-leaf (tree-seq-breadth (deeply-nested 10000))))))
  ;test laziness
  ;level 0 (root)
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 0 :not-found))))
  ;level 1
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 1 :not-found))))
  (is (= '[((:x) :b) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 2 :not-found))))
  (is (= '[:c 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 3 :not-found))))
  (is (= '[((:y) :d) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 4 :not-found))))
  (is (= '[:e 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 5 :not-found))))
  ;level 2
  (is (= '[(:x) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 6 :not-found))))
  (is (= '[:b 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 7 :not-found))))
  (is (= '[(:y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x) 
            ((:y) :d) 
            :e]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 8 :not-found))))
  (is (= '[:d 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x) 
            ((:y) :d) :e]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 9 :not-found))))
  ;level 3
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x) 
            ((:y) :d) 
            :e 
            :b 
            :x]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 10 :not-found))))
  (is (= '[:y 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x) 
            ((:y) :d) 
            :e 
            :b 
            :x 
            (:y) 
            :d]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 11 :not-found))))
  ;not found
  (is (= '[:not-found 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a 
            ((:x) :b) 
            :c 
            (:x) 
            ((:y) :d) 
            :e 
            :b 
            :x 
            (:y) 
            :d 
            :y]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e)) 12 :not-found))))
  ;all
  (is (= '[(
            (:a ((:x) :b) :c ((:y) :d) :e)
            :a ((:x) :b) :c ((:y) :d) :e
            (:x) :b (:y) :d
            :x :y
            ) 
           [(:a ((:x) :b) :c ((:y) :d) :e) 
            :a ((:x) :b) :c (:x) ((:y) :d) :e :b :x (:y) :d :y]] 
         (recorder get-children get-child-seq (doall (tree-seq-breadth '(:a ((:x) :b) :c ((:y) :d) :e))))))
  )

(deftest test-tree-seq-breadth-x
  (binding 
    [tree-seq-breadth tree-seq-breadth-x]
    (do-base-test-tree-seq-breadth)
  )
  ;test that no stack overflow; passes 100000
  (is (= '(:bottom) (doall (filter is-leaf (tree-seq-breadth-x (deeply-nested 10000))))))
  ;test laziness
  ;level 0 (root)
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) []] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 0 :not-found))))
  ;level 1
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 1 :not-found))))
  (is (= '[((:x) :b) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 2 :not-found))))
  (is (= '[:c 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 3 :not-found))))
  (is (= '[((:y) :d) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 4 :not-found))))
  (is (= '[:e 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 5 :not-found))))
  ;level 2
  (is (= '[(:x) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 6 :not-found))))
  (is (= '[:b 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 7 :not-found))))
  (is (= '[(:y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 8 :not-found))))
  (is (= '[:d 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 9 :not-found))))
  ;level 3
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 10 :not-found))))
  (is (= '[:y 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y)]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 11 :not-found))))
  ;not found
  (is (= '[:not-found 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y) :d :x :y]] 
         (recorder get-children get-child-seq (nth (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e)) 12 :not-found))))
  ;all
  (is (= '[(
            (:a ((:x) :b) :c ((:y) :d) :e)
            :a ((:x) :b) :c ((:y) :d) :e
            (:x) :b (:y) :d
            :x :y
            ) 
           [(:a ((:x) :b) :c ((:y) :d) :e) :a ((:x) :b) :c ((:y) :d) :e (:x) :b (:y) :d :x :y]] 
         (recorder get-children get-child-seq (doall (tree-seq-breadth-x '(:a ((:x) :b) :c ((:y) :d) :e))))))
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
  ;passes 10000
  (is (= '((:bottom)) (get-nodes-at-depth (deeply-nested 10000) 10000)))
  ;TEST LAZINESS FOR doall
  ;level 0 (root)
  (is (= '[((:a ((:x) :b) :c ((:y) :d) :e)) 
           []] 
         (recorder get-children get-child-seq (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 0)))))
  ;level 1
  (is (= '[(:a ((:x) :b) :c ((:y) :d) :e) 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 1)))))
  ;level 2
  (is (= '[((:x) :b (:y) :d) 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d)]] 
         (recorder get-children get-child-seq (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2)))))
  ;level 3
  (is (= '[(:x :y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 3)))))
  ;level 4
  (is (= '[() 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (doall (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 4)))))
  ;TEST LAZINESS FOR nth
  ;level 1
  (is (= '[:a 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 1) 0))))
  (is (= '[:e 
           [(:a ((:x) :b) :c ((:y) :d) :e)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 1) 4))))
  ;level 2
  (is (= '[(:x) 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2) 0))))
  (is (= '[:b 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2) 1))))
  (is (= '[(:y) 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2) 2))))
  (is (= '[:d  
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) ((:y) :d)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 2) 3))))
  ;level 3
  (is (= '[:x 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 3) 0))))
  (is (= '[:y 
           [(:a ((:x) :b) :c ((:y) :d) :e) ((:x) :b) (:x) ((:y) :d) (:y)]] 
         (recorder get-children get-child-seq (nth (get-nodes-at-depth '(:a ((:x) :b) :c ((:y) :d) :e) 3) 1))))
)

(deftest test-get-nodes-at-depth-series
  ;fails - Stack Overflow at 10000
  (is (= '((:bottom)) (nth (get-nodes-at-depth-series (deeply-nested 10000)) 10000)))
)

(deftest test-tree-seq-with-treenode-fixed
  (let [
        tn031 (create-fixed-node :tn031 nil)
        tn012 (create-fixed-node :tn012 nil)
        tn011 (create-fixed-node :tn011 nil)
        tn03  (create-fixed-node :tn03  [tn031])
        tn02  (create-fixed-node :tn02  nil)
        tn01  (create-fixed-node :tn01  [tn011 tn012])
        tn0   (create-fixed-node :tn0   [tn01 tn02 tn03])]
    (is (= false (is-leaf tn0)))
    (is (= [tn01 tn02 tn03] (get-children tn0)))
    (is (= [tn0 tn01 tn011 tn012 tn02 tn03 tn031] (tree-seq-depth tn0)))
    (is (= [tn0 tn01 tn02 tn03 tn011 tn012 tn031] (tree-seq-breadth tn0)))
   )
  )

(deftest test-tree-seq-with-treenode-dynamic
    (is (= '(:a ((:x) :b) :c ((:y) :d) :e) 
           (map #(node-get-value %) (get-children (create-tree-node-dynamic '(:a ((:x) :b) :c ((:y) :d) :e))))))
  )

(defn node-mapping-fn 
  [node]
  (node-get-value node))

(defn node-mapping-fn-gc-sleep 
  [node]
  (let [value (node-get-value node)] 
    (println (java.util.Date.) " RETURN " value) 
    (System/gc) 
    (Thread/sleep 2000) 
    value)
  )

(defn test-tree-seq-with-treenode-dynamic-finalization-1 
  []
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

(defn test-tree-seq-with-treenode-dynamic-finalization-2 
  []
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

(defn perform-test-finalize-tree-seq-x 
  [tree-seq-fn root-child-value-seq]
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

(defn perform-test-finalize-tree-seq-breadth 
  [] 
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

(deftest test-tree-seq-multi-depth
  (is (= '([() ()]) (doall (tree-seq-multi-depth '() '()))))
  (is (= '([:a :A]) (doall (tree-seq-multi-depth :a :A))))
  (is (= '([(:a) (:A)] [:a :A]) (doall (tree-seq-multi-depth '(:a) '(:A)))))
  (is (= '([(:a (:b) :c) (:A :B :C)] 
            [:a :A]
            [(:b) :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth '(:a (:b) :c) '(:A :B :C)))))
  (is (= '([(:a (:b) :c) (:A (:B) :C)] 
            [:a :A]
            [(:b) (:B)]
            [:b :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth '(:a (:b) :c) '(:A (:B) :C)))))
  (is (= '([(:a ((:b)) :c) (:A (:B) :C)] 
            [:a :A]
            [((:b)) (:B)]
            [(:b) :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth '(:a ((:b)) :c) '(:A (:B) :C)))))
  (is (= ['?x :x] (nth (tree-seq-multi-depth (deeply-nested 10000 '?x) (deeply-nested 10000 :x)) 10000)))
  )

(deftest test-tree-seq-multi-depth-leaves
  (is (= '([() ()]) (doall (tree-seq-multi-depth-leaves '() '()))))
  (is (= '([:a :A]) (doall (tree-seq-multi-depth-leaves :a :A))))
  (is (= '([:a :A]) (doall (tree-seq-multi-depth-leaves '(:a) '(:A)))))
  (is (= '( [:a :A]
            [(:b) :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth-leaves '(:a (:b) :c) '(:A :B :C)))))
  (is (= '( [:a :A]
            [:b :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth-leaves '(:a (:b) :c) '(:A (:B) :C)))))
  (is (= '( [:a :A]
            [(:b) :B]
            [:c :C]
            ) 
         (doall (tree-seq-multi-depth-leaves '(:a ((:b)) :c) '(:A (:B) :C)))))
  (is (= ['?x :x] (first (tree-seq-multi-depth-leaves (deeply-nested 10000 '?x) (deeply-nested 10000 :x)))))
  )

(run-tests)
