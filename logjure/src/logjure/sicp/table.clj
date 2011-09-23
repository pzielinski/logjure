(ns logjure.sicp.table
  (:use 
    logjure.sicp.base 
    logjure.sicp.pair
    )
  )

(def *store* (atom {}))

(defn get-from-table 
  [ & the-keys ]
  (get @*store* the-keys)
  )

(defn put 
  [value & the-keys]
  (swap! *store* assoc the-keys value)
  )

