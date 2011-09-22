(ns logjure.sicp.base)

;---------------------------------------------------------------------------------------------------
; CLOJURE INTERFACE

(defn display [& more]
  (println more)
  )

(defn error [& more]
  (display more)
  )

(defn eq? [s1 s2]
  (= s1 s2)
  )

(defn equal? [list1 list2]
  (= list1 list2)
  )

(defn null? [x]
  (nil? x)
  )

(defn string-append [& strs]
  (apply str strs)
  )

(defn string->symbol [str]
  (symbol str)
  )

(defn symbol->string [symbol]
  (str symbol)
  )

(defn number->string [n]
  (str n)
  )

(defn substring [s start end]
  (subs s start end)
  )

(defn string=? [str1 str2]
  (= str1 str2)
  )

(defn string-length [str]
  (.length str)
  )

