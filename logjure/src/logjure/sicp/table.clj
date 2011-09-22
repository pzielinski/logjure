(ns logjure.sicp.table
  (:use logjure.sicp.base logjure.sicp.pair)
  )

;---------------------------------------------------------------------------------------------------
; MAP/TABLE EMULATION WITH PAIR

(defn make-table []
  (list '*table*)
  )

(defn lookup [key-1 key-2 table]
  (let [subtable (assoc- key-1 (cdr table))]
    (if subtable
      (let [record (assoc- key-2 (cdr subtable))]
        (if record
          (cdr record)
          false))
      false))
  )

(defn insert! [key-1 key-2 value table]
  (let [subtable (assoc- key-1 (cdr table))]
    (if subtable
      (let [record (assoc- key-2 (cdr subtable))]
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons-pair (cons-pair key-2 value) (cdr subtable)))))
      (set-cdr! table
                (cons-pair 
                  (list key-1 (cons-pair key-2 value));USES LIST explicitely !!!!!!!!!!!!!!!!!!!!!!!
                  (cdr table)))))
  'ok
  );table has to be an ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn make-table-local []
  (let [local-table (make-table)]
    (fn dispatch [m]
      (cond (eq? m 'lookup-proc) 
            (fn [key-1 key-2] (lookup key-1 key-2 local-table))
            (eq? m 'insert-proc!)
            (fn [key-1 key-2 value] (insert! key-1 key-2 value local-table))
            :else 
            (error "Unknown operation -- TABLE" m))))
  )

(defn operation-table [dispatch-value]
  ((make-table-local) dispatch-value)
  )

(defn get-from-table [key-1 key-2] 
  ((operation-table 'lookup-proc) key-1 key-2)
  )

;this will not work! operation-table will create new table!!!!!!!!!!!!!!
(defn put [key-1 key-2 value] 
  ((operation-table 'insert-proc!) key-1 key-2 value) 
  )

