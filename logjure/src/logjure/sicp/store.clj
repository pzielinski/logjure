(ns logjure.sicp.store
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    logjure.sicp.stream
    logjure.sicp.table
    logjure.sicp.syntax
    )
  )

;---------------------------------------------------------------------------------------------------
; ASSERTIONS & RULES DATABASE

(def THE-ASSERTIONS the-empty-stream);SHOULD BE AN ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn get-all-assertions [] 
  THE-ASSERTIONS
  )

(defn indexable? [pat]
  (or (constant-symbol? (car pat)) (variable? (car pat)))
  )

(defn use-index? [pat]
  (constant-symbol? (car pat))
  )

(defn index-key-of [pat]
  (let [key (car pat)]
    (if (variable? key) 
      '? 
      key))
  )

(defn get-stream [key1 key2]
  (let [s (get-from-table key1 key2)];get-from-table needs a table !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    (if s s 
      the-empty-stream))
  )

(defn get-indexed-assertions [pattern]
  (get-stream (index-key-of pattern) 'assertion-stream)
  )

(defn fetch-assertions
  "One important problem in designing logic programming languages is that of arranging things so that as few
irrelevant data-base entries as possible will be examined in checking a given pattern. In our system, in addition to
storing all assertions in one big stream, we store all assertions whose cars are constant symbols in separate streams,
in a table indexed by the symbol. To fetch an assertion that may match a pattern, we first check to see if the car of
the pattern is a constant symbol. If so, we return (to be tested using the matcher) all the stored assertions that have
the same car. If the pattern's car is not a constant symbol, we return all the stored assertions. Cleverer methods
could also take advantage of information in the frame, or try also to optimize the case where the car of the pattern
is not a constant symbol. We avoid building our criteria for indexing (using the car, handling only the case of
constant symbols) into the program; instead we call on predicates and selectors that embody our criteria."
  [pattern frame]
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions))
  )

(def THE-RULES the-empty-stream);SHOULD BE AN ATOM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defn get-all-rules [] 
  THE-RULES
  )

(defn get-indexed-rules [pattern]
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream))
  )

(defn fetch-rules
  "Rules are stored similarly, using the car of the rule conclusion. Rule conclusions are arbitrary patterns, however, so
they differ from assertions in that they can contain variables. A pattern whose car is a constant symbol can match
rules whose conclusions start with a variable as well as rules whose conclusions have the same car. Thus, when
fetching rules that might match a pattern whose car is a constant symbol we fetch all rules whose conclusions start
with a variable as well as those whose conclusions have the same car as the pattern. For this purpose we store all
rules whose conclusions start with a variable in a separate stream in our table, indexed by the symbol ?."
  [pattern frame]
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules))
  )

(defn store-assertion-in-index [assertion]
  (if (indexable? assertion)
    (let [key (index-key-of assertion)]
      (let [current-assertion-stream (get-stream key 'assertion-stream)]
        (put 
          key 
          'assertion-stream
          (cons-stream assertion current-assertion-stream)))))
  )

(defn store-rule-in-index [rule]
  (let [pattern (conclusion rule)]
    (if (indexable? pattern)
      (let [key (index-key-of pattern)]
        (let [current-rule-stream (get-stream key 'rule-stream)]
          (put 
            key
            'rule-stream
            (cons-stream rule current-rule-stream))))))
  )

(defn add-assertion! [assertion]
  (store-assertion-in-index assertion)
  (let [old-assertions THE-ASSERTIONS]
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions));ATOM!!!!!!!!!!!!
    'ok)
  )

(defn add-rule! [rule]
  (store-rule-in-index rule)
  (let [old-rules THE-RULES]
    (set! THE-RULES (cons-stream rule old-rules));ATOM!!!!!!!!!!!!
    'ok)
  )

(defn add-rule-or-assertion! [assertion]
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion))
  )
