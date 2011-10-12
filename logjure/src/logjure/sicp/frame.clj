(ns logjure.sicp.frame
  )

(defn make-empty-frame 
  []
  {}
  )

(defn get-value-in-frame 
  [variable frame]
  (get frame variable)
  )

(defn extend-frame 
  [variable value frame]
  (assoc frame variable value))

(defn combine-frames 
  [frame1 frame2]
  (merge frame1 frame2)
  )

(defn map2frame
  [map]
  map
  )