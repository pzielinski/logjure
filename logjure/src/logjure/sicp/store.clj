(ns logjure.sicp.store
  (:use 
    clojure.set
    logjure.sicp.table
    logjure.sicp.syntax
    )
  )

(defn- add-to-stream 
  [x stream]
  (union #{x} stream)
  )

(defn- get-stream 
  [key1 key2]
  (let [s (get-from-table key1 key2)]
    (if s 
      s 
      #{}))
  )

(defn- get-all-assertions 
  [] 
  (get-stream 'all-assertions 'assertion-stream)
  )

(defn- indexable? 
  "used when adding assertions/rules to store"
  [pat]
  (or (constant-symbol? (first pat)) (variable? (first pat)))
  )

(defn- use-index? 
  "used when getting assertions/rules from store"
  [pat]
  (constant-symbol? (first pat))
  )

(defn- index-key-of 
  [pat]
  (let [key (first pat)]
    (if (variable? key) 
      '? 
      key))
  )

(defn- get-indexed-assertions
  [pattern]
  (get-stream (index-key-of pattern) 'assertion-stream)
  )

(defn get-assertions
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

(defn- store-assertion-in-all 
  [assertion]
  (put
    (add-to-stream assertion (get-all-assertions))
    'all-assertions
    'assertion-stream
    )
  'ok
  )

(defn- store-assertion-in-index 
  [assertion]
  (let [key (index-key-of assertion)]
    (let [current-assertion-stream (get-stream key 'assertion-stream)]
      (put
        (add-to-stream assertion current-assertion-stream)
        key 
        'assertion-stream
        )))
  )

(defn add-assertion! 
  [assertion]
  (if (indexable? assertion)
    (store-assertion-in-index assertion)
    (store-assertion-in-all assertion))
  )

(defn- get-all-rules 
  [] 
  (get-stream 'all-rules 'rule-stream)
  )

(defn- get-indexed-rules [pattern]
  (union
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream))
  )

(defn get-rules
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

(defn- store-rule-in-all 
  [rule]
  (let [pattern (conclusion rule)]
    (put
      (add-to-stream rule (get-all-rules))
      'all-rules
      'rule-stream
      ))
  )

(defn- store-rule-in-index 
  [rule]
  (let [pattern (conclusion rule)]
    (let [the-key (index-key-of pattern)]
      (let [current-rule-stream (get-stream the-key 'rule-stream)]
        (put
          (add-to-stream rule current-rule-stream)
          the-key
          'rule-stream
          ))))
  )

(defn add-rule! 
  [rule]
  (if (indexable? (conclusion rule))
    (store-rule-in-index rule)
    (store-rule-in-all rule))
  )

(defn add-rule-or-assertion! 
  [assertion]
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion))
  )
