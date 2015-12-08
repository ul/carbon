(ns carbon.vdom.hook)

(deftype Hook [hook! unhook! args]
  Object
  (data [_] args)
  (hook [_ node prop prev]
    (hook! node prop prev))
  (unhook [_ node prop next]
    (unhook! node prop next)))

(def noop (constantly nil))

(defn hook
  ([hook!] (Hook. hook! noop nil))
  ([hook! unhook!] (Hook. hook! unhook! nil))
  ([hook! unhook! data] (Hook. hook! unhook! data)))

(defn unhook
  ([unhook!] (Hook. noop unhook! nil))
  ([unhook! data] (Hook. noop unhook! data)))

(goog/exportSymbol "Hook" Hook)
(goog/exportSymbol "Hook.prototype.hook" Hook.prototype.hook)
(goog/exportSymbol "Hook.prototype.unhook" Hook.prototype.unhook)