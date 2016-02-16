(ns carbon.vdom.hook)

(deftype Hook [h u a]

  Object
  (hook [this node prop prev]
    (when h (h this node prop prev)))
  (unhook [this node prop next]
    (when u (u this node prop next)))

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

(aset Hook "prototype" "type" "Hook")

(goog/exportSymbol "Hook" Hook)
(goog/exportSymbol "Hook.prototype.hook" Hook.prototype.hook)
(goog/exportSymbol "Hook.prototype.unhook" Hook.prototype.unhook)

(defn hook
  ([h] (hook h nil nil))
  ([h u] (hook h u nil))
  ([h u a] (Hook. h u (atom a))))

(defn unhook
  ([u] (hook nil u nil))
  ([u a] (hook nil u a)))