(ns logjure.sicp.stream
  (:use logjure.sicp.pair)
  )

(def the-empty-stream nil)

(defn stream-null? [stream]
  (null? stream)
  )

(defmacro cons-stream [x s]
    `(cons-pair ~x (delay ~s))
  )

(defn stream-car [stream] 
  (car stream)
  )

(defn stream-cdr [stream] 
  (force (cdr stream))
  )

(defn singleton-stream [x]
  (cons-stream x the-empty-stream)
  )

(defn stream-append [s1 s2]
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (stream-append (stream-cdr s1) s2)));NEED RECUR !!!!!!!!!!!!!
  )

(defn stream-append-delayed [s1 delayed-s2]
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (stream-append-delayed (stream-cdr s1) delayed-s2)));NEED RECUR !!!!!!!!!!!!!
  )

(defn interleave-delayed [s1 delayed-s2]
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream
      (stream-car s1)
      (interleave-delayed (force delayed-s2) (delay (stream-cdr s1)))));NEED RECUR !!!!!!!!!!!!!
  )

(defn flatten-stream [stream]
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream)))));NEED RECUR !!!!!!!!!!!!!
  )

(defn stream-map [proc stream]
  (if (stream-null? stream)
    the-empty-stream
    (cons-stream 
      (proc (stream-car stream)) 
      (stream-map proc (stream-cdr stream))));NEED RECUR !!!!!!!!!!!!!
  )

(defn stream-flatmap [proc stream]
  (flatten-stream (stream-map proc stream))
  )

(defn stream-nth [n stream]
  (if (stream-null? stream)
    the-empty-stream
    (if (= n 0)
      (stream-car stream) 
      (stream-nth (dec n) (stream-cdr stream))));NEED RECUR !!!!!!!!!!!!!
  )

(defn seq-to-stream [the-seq]
  (if (seq the-seq)
    (cons-stream (first the-seq) (seq-to-stream (rest the-seq)))
    the-empty-stream)
  )