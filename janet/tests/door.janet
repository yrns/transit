# a door

(defn intact-enter [self door event]
  (print "You are in front of a large wooden door."))

(defn closed-enter [self door event]
  (print "The door is now closed."))

(defn open-enter [self door event]
  (print "The door is now open."))

(defn destroyed-enter [self door event]
  (print "The door shatters into many pieces."))

(defn locked-enter [self door event]
  (when event 
    (print "The door is now locked.")))

(defn bash-guard [self door damage]
  (let [hp (get door :hit-points)
        cur (- (get hp :current) damage)
        destroyed (<= cur 0)]
    (put hp :current (max cur 0))
    (unless destroyed
      (print "The door appears to be damaged."))
    destroyed))

(defn lock-guard [self door event]
  (= (get door :key) event))

(defn unlock-guard [self door event]
  (= (get door :key) event))

(defn destroyed-exit [self ctx ev]
  # never
  )
