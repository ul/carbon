(ns carbon.vdom.widget)

(deftype Widget [i u d a]

  Object
  (init [this] (i this))
  (update [this prev node] (u this prev node))
  (destroy [this node] (d this node))

  IDeref
  (-deref [_]
    @a)

  IReset
  (-reset! [_ new-value]
    (reset! a new-value))

  ISwap
  (-swap! [_ f]
    (swap! a f))
  (-swap! [_ f x]
    (swap! a f x))
  (-swap! [_ f x y]
    (swap! a f x y))
  (-swap! [_ f x y xs]
    (apply swap! a f x y xs)))

(aset Widget "prototype" "type" "Widget")

(goog/exportSymbol "Widget" Widget)
(goog/exportSymbol "Widget.prototype.type" Widget.prototype.type)
(goog/exportSymbol "Widget.prototype.init" Widget.prototype.init)
(goog/exportSymbol "Widget.prototype.update" Widget.prototype.update)
(goog/exportSymbol "Widget.prototype.destroy" Widget.prototype.destroy)


(defn widget
  ([i u d] (widget i u d nil))
  ([i u d a] (Widget. i u d (atom a))))