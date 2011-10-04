(ns logjure.sicp.stream-test
  (:use 
    logjure.sicp.pair
    logjure.sicp.stream
    clojure.contrib.test-is
    )
  )

(deftest test-empty-stream
  (is (= nil the-empty-stream))
)

(deftest test-stream-null?
  (is (= true (stream-null? nil)))
)

(deftest test-cons-stream
  (is (= :a (stream-car (cons-stream :a the-empty-stream))))
  (is (= the-empty-stream (stream-cdr (cons-stream :a the-empty-stream))))
)

(deftest test-singleton-stream
  (is (= :a (stream-car (singleton-stream :a))))
  (is (= the-empty-stream (stream-cdr (singleton-stream :a))))
)

(deftest test-stream-append
  (let [s (stream-append (cons-stream :a (singleton-stream :b)) (singleton-stream :x))] 
    (is (= :a (stream-car s)))
    (is (= :b (stream-car (stream-cdr s))))
    (is (= :x (stream-car (stream-cdr (stream-cdr s)))))
    (is (= the-empty-stream (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
  )
)

(deftest test-stream-append-delayed
  (let [s (stream-append-delayed (cons-stream :a (singleton-stream :b)) (singleton-stream :x))] 
    (is (= :a (stream-car s)))
    (is (= :b (stream-car (stream-cdr s))))
    (is (= :x (stream-car (stream-cdr (stream-cdr s)))))
  )
)

(deftest test-interleave-delayed
  (let [s1 (cons-stream :a (cons-stream :b (cons-stream :c (singleton-stream :d))))
        s2 (cons-stream :x (cons-stream :y (cons-stream :z (singleton-stream :zz))))
        s (interleave-delayed s1 s2)
        ] 
    (is (= :a (stream-car s)))
    (is (= :x (stream-car (stream-cdr s))))
    (is (= :b (stream-car (stream-cdr (stream-cdr s)))))
    (is (= :y (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
    (is (= :c (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))
    (is (= :z (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))
    (is (= :d (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))
    (is (= :zz (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))))
    (is (= the-empty-stream (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))))
    )
  )

(deftest test-flatten-stream
  (let [s1 (cons-stream :a (cons-stream :b (cons-stream :c (singleton-stream :d))))
        s2 (cons-stream :x (cons-stream :y (cons-stream :z (singleton-stream :zz))))
        sX (cons-stream s1 (singleton-stream s2));stream of streams
        s (flatten-stream sX)
        ] 
    (is (= :a (stream-car s)))
    (is (= :x (stream-car (stream-cdr s))))
    (is (= :b (stream-car (stream-cdr (stream-cdr s)))))
    (is (= :y (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
    (is (= :c (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))
    (is (= :z (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))
    (is (= :d (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))
    (is (= :zz (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))))
    (is (= the-empty-stream (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))))
    )
  )

(deftest test-flatten-stream-3
  (let [s1 (cons-stream :a (cons-stream :b (singleton-stream :c)))
        s2 (cons-stream :x (cons-stream :y (singleton-stream :z)))
        s3 (cons-stream 1 (cons-stream 2 (singleton-stream 3)))
        sX (cons-stream s1 (cons-stream s2 (singleton-stream s3)));stream of streams
        s (flatten-stream sX)
        ] 
    (is (= :a (stream-car s)))
    (is (= :x (stream-car (stream-cdr s))))
    (is (= :b (stream-car (stream-cdr (stream-cdr s)))))
    (is (= 1 (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
    (is (= :c (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))
    (is (= :y (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))
    (is (= 2 (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))
    (is (= :z (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))))
    (is (= 3 (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))))
    (is (= the-empty-stream (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))))))
    )
  )

(deftest test-stream-map
  (let [s (stream-map #(+ % 10) (cons-stream 1 (cons-stream 2 (singleton-stream 3))))] 
    (is (= 11 (stream-car s)))
    (is (= 12 (stream-car (stream-cdr s))))
    (is (= 13 (stream-car (stream-cdr (stream-cdr s)))))
  )
)

(deftest test-stream-flatmap
  (let [s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 (singleton-stream 4))))
        s (stream-flatmap #(cons-stream % s1) s1)
        ;1 1 2 3 4
        ;2 1 2 3 4
        ;3 1 2 3 4
        ;4 1 2 3 4
        ] 
    (is (= 1 (stream-car s)))
    (is (= 2 (stream-car (stream-cdr s))))
    (is (= 1 (stream-car (stream-cdr (stream-cdr s)))))
    (is (= 3 (stream-car (stream-cdr (stream-cdr (stream-cdr s))))))
    (is (= 2 (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))
    (is (= 1 (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s))))))))
    (is (= 3 (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr s)))))))))
    )
  )

(deftest test-stream-nth
  (let [s (cons-stream :a (cons-stream :b (singleton-stream :c)))] 
    (is (= :a (stream-nth 0 s)))
    (is (= :b (stream-nth 1 s)))
    (is (= :c (stream-nth 2 s)))
    (is (= the-empty-stream (stream-nth 3 s)))
  )
)

(defn deeply-nested [n]
  (loop [n n result '(:bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))

(defn make-lazy-int-stream [n]
  (cons-stream n (delay (make-lazy-int-stream (inc n))))
  )

(deftest test-stream-nth
  (let [s (cons-stream :a (cons-stream :b (singleton-stream :c)))] 
    (is (= :a (stream-nth 0 s)))
    (is (= :b (stream-nth 1 s)))
    (is (= :c (stream-nth 2 s)))
    (is (= the-empty-stream (stream-nth 3 s)))
  )
)

(deftest test-seq-to-stream
  (let [s (seq-to-stream '(1 2 3))] 
    (is (= 1 (stream-nth 0 s)))
    (is (= 2 (stream-nth 1 s)))
    (is (= 3 (stream-nth 2 s)))
    (is (= the-empty-stream (stream-nth 3 s)))
    (is (= 1 (stream-nth 0 (seq-to-stream (iterate inc 1)))))
    (is (= 2 (stream-nth 1 (seq-to-stream (iterate inc 1)))))
    (is (= 10001 (stream-nth 10000 (seq-to-stream (iterate inc 1)))))
  )
)

(run-tests)
