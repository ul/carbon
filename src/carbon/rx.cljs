(ns carbon.rx)

(defprotocol IReactiveSource
  (get-rank [_])
  (add-sink [_ sink])
  (remove-sink [_ sink])
  (get-sinks [_]))

(defprotocol IReactiveExpression
  (compute [_])
  (gc [_])
  (add-source [_ source])
  (remove-source [_ source]))

(def ^:dynamic *rx* nil)                                    ; current parent expression
(def ^:dynamic *rank* nil)                                  ; highest rank met during expression compute
(def ^:dynamic *dirty-sinks* nil)                           ; subject to `compute`
(def ^:dynamic *dirty-sources* nil)                         ; subject to `gc`
(def ^:dynamic *provenance* [])

(defn compare-by [keyfn]
  (fn [x y]
    (compare (keyfn x) (keyfn y))))

(defn rank-hash [x]
  [(get-rank x) (hash x)])

(def empty-queue (sorted-set-by (compare-by rank-hash)))

(defn propagate
  "Recursively compute all dirty sinks in the `queue` and return all visited sources to clean."
  [queue]
  (binding [*rx* nil *rank* nil]                            ; try to be foolproof
    (loop [queue queue dirty '()]
      (if-let [x (first queue)]
        (let [queue (disj queue x)]
          (recur (if (= @x (compute x)) queue (->> x get-sinks (into queue)))
                 (conj dirty x)))
        dirty))))

(defn clean
  "Recursively garbage collect all disconnected sources in the `queue`"
  [queue]
  (doseq [source queue]
    (gc source)))

(defn register [source]
  (when *rx*                                                ; *rank* too
    (add-sink source *rx*)
    (add-source *rx* source)
    (vswap! *rank* max (get-rank source))))

(defn dosync* [f]
  (let [sinks (or *dirty-sinks* (volatile! empty-queue))
        sources (or *dirty-sources* (volatile! empty-queue))
        result (binding [*dirty-sinks* sinks
                         *dirty-sources* sources]
                 (f))]
    (binding [*dirty-sources* sources]
      (vswap! *dirty-sources* into (propagate @sinks)))
    (when-not *dirty-sources*
      (clean @sources))
    result))

(deftype ReactiveExpression [getter setter meta validator drop
                             ^:mutable state ^:mutable watches
                             ^:mutable rank ^:mutable sources ^:mutable sinks]

  Object
  (equiv [this other] (-equiv this other))

  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [this]
    (when (= state ::thunk) (compute this))
    (register this)
    state)

  IReactiveSource
  (get-rank [_] rank)
  (add-sink [_ sink] (set! sinks (conj sinks sink)))
  (remove-sink [_ sink] (set! sinks (disj sinks sink)))
  (get-sinks [_] sinks)

  IReactiveExpression
  (compute [this]
    (doseq [source sources]
      (remove-sink source this))
    (set! sources #{})
    (let [old-value state
          r (volatile! 0)
          new-value (binding [*rx* this
                              *rank* r
                              *provenance* (conj *provenance* this)]
                      (getter))]
      (set! rank (inc @r))
      (when (not= old-value new-value)
        (set! state new-value)
        (-notify-watches this old-value new-value))
      new-value))
  (gc [this]
    (if *dirty-sources*
      (vswap! *dirty-sources* conj this)
      (when (and (empty? sinks) (empty? watches))
        (doseq [source sources]
          (remove-sink source this)
          (when (satisfies? IReactiveExpression source)
            (gc source)))
        (set! sources #{})
        (set! state ::thunk)
        (when drop (drop)))))
  (add-source [_ source]
    (set! sources (conj sources source)))
  (remove-source [_ source]
    (set! sources (disj sources source)))

  IMeta
  (-meta [_] meta)

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (when (= state ::thunk) (compute this))
    (set! watches (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! watches (dissoc watches key))
    (gc this)
    this)

  IHash
  (-hash [this] (goog/getUid this))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#<RLens: ")
    (pr-writer state writer opts)
    (-write writer ">"))

  IReset
  (-reset! [_ new-value]
    (assert setter "Can't reset lens w/o setter")
    (when-not (nil? validator)
      (assert (validator new-value) "Validator rejected reference state"))
    (dosync* #(setter new-value))
    new-value)

  ISwap
  (-swap! [this f]
    (when (= state ::thunk) (compute this))
    (reset! this (f state)))
  (-swap! [this f x]
    (when (= state ::thunk) (compute this))
    (reset! this (f state x)))
  (-swap! [this f x y]
    (when (= state ::thunk) (compute this))
    (reset! this (f state x y)))
  (-swap! [this f x y xs]
    (when (= state ::thunk) (compute this))
    (reset! this (apply f state x y xs))))

(defn watch [_ source o n]
  (when (not= o n)
    (if *dirty-sinks*
      (vswap! *dirty-sinks* into (get-sinks source))
      (->> source get-sinks (into empty-queue) propagate clean))))

(defn cell [x & m]
  (let [sinks (volatile! #{})]
    (specify! (apply atom x m)

      IReactiveSource
      (get-rank [_] 0)
      (add-sink [_ sink] (vswap! sinks conj sink))
      (remove-sink [_ sink] (vswap! sinks disj sink))
      (get-sinks [_] @sinks)

      IDeref
      (-deref [this]
        (register this)
        (add-watch this ::rx watch)
        (.-state this)))))

(def $ cell)

(defn rx*
  ([getter] (rx* getter nil nil nil nil))
  ([getter setter] (rx* getter setter nil nil nil))
  ([getter setter meta] (rx* getter setter meta nil nil))
  ([getter setter meta validator] (rx* getter setter meta validator nil))
  ([getter setter meta validator drop]
   (ReactiveExpression. getter setter meta validator drop ::thunk {} 0 #{} #{})))