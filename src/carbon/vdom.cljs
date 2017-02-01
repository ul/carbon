(ns carbon.vdom
  (:require [incremental-dom :as dom]
            [cuerdas.core :as str]
            [carbon.rx :as rx :include-macros true]))

(def tree
  "{p :children {a :children {t :children {h
   {:node :view :f :args :meta :children {}}}}}}"
  (volatile! {}))

(def component-lifecycle
  "Lifecycle callbacks supported via component metadata."
  [:component-will-mount
   :component-did-mount
   :component-should-update
   :component-will-receive-props
   :component-will-update
   :component-did-update
   :component-will-unmount
   :component-did-unmount])

(def component-meta
  "Keys supported in component metadata."
  (conj component-lifecycle :key))

(defn map-map
  "Returns a map with `fk` applied to each key
  and `fv` applied to each value of `m`."
  [fk fv m]
  (persistent!
   (reduce-kv
    (fn [m k v] (assoc! m (fk k) (fv v)))
    (transient {})
    m)))

(defn map-keys
  "Returns a map with `f` applied to each key of `m`."
  [f m]
  (map-map f identity m))

(defn map-vals
  "Returns a map with `f` applied to each value of `m`."
  [f m]
  (map-map identity f m))

(defn filter-vals
  "Returns a map with only entries for which `(p val)` is truthy."
  [p m]
  (persistent!
   (reduce-kv
    (fn [m k v] (if (p v) (assoc! m k v) m))
    (transient {})
    m)))

(defn deref-if-rx [x]
  (if (satisfies? rx/IReactiveSource x) @x x))

(defn flatten-children [children]
  (->> children
       (tree-seq seq? seq)
       (remove seq?)
       (remove nil?)))

(defn map->js [x]
  (if (map? x)
    (let [y (js-obj)]
      (doseq [[k v] x :when (some? v) :let [v (deref-if-rx v)]]
        (aset y (name k) (if (map? v) (map->js v) v)))
      y)
    x))

(defn valid-tag? [tag]
  (or (keyword? tag) (string? tag)))

(defn parse-selector [s]
  (loop [matches (re-seq #"([#.])?([^#.]+)" (name s))
         tag     "div"
         id      nil
         classes nil]
    (if-let [[_ prefix val] (first matches)]
      (case prefix
        nil (recur (next matches) val id classes)
        "#" (recur (next matches) tag val classes)
        "." (recur (next matches) tag id (conj (or classes []) val)))
      [tag id classes])))

(defn normalize-element [[first second & rest :as arg]]
  (let [[tag tag-id tag-classes] (parse-selector first)
        [attrs children] (if (or (map? second)
                                 (nil? second))
                           [second rest]
                           [{}     (cons second rest)])
        attrs-classes    (get attrs :class)
        classes          (if (and tag-classes attrs-classes)
                           [tag-classes attrs-classes]
                           (or tag-classes attrs-classes))
        attrs-id         (get attrs :id)
        attrs            (assoc attrs
                             :key   (get (meta arg) :key)
                             :class (->> classes flatten (str/join " "))
                             :id    (or attrs-id tag-id))]
    [tag attrs children]))

(declare process component)

(defn node [[tag attrs children] path]
  (let [key (get attrs :key)
        path (conj path key)
        attrs (->> (apply dissoc attrs component-meta)
                   (filter-vals some?)
                   (map-map name map->js)
                   seq
                   flatten)
        tag (name tag)
        node (apply dom/elementOpen tag key nil attrs)]
    (->> children
         flatten-children
         (map-indexed
          (fn [i c] (process c (conj path (-> c meta (get :key i))))))
         dorun)
    (dom/elementClose tag)
    node))

(defn process [arg path]
  (cond
    (vector? arg)
    (let [tag (get arg 0)]
      (if (valid-tag? tag)
        (node (normalize-element arg) path)
        (component tag (subvec arg 1) (meta arg) path)))

    (seq? arg)
    (node [:div {} arg] path)

    :else
    (dom/text (str arg))))

;; FIXME put components' key into its top-level element
(defn make-component [f args meta path]
  (let [form (rx/no-rx (apply f args))
        form-2? (fn? form)
        view (if form-2? form f)
        args (rx/cell args)
        view (rx/rx (apply view @args))
        node (process (if form-2? @view form) path)
        path' (vec (interpose :children path))]
    (vswap! tree assoc-in path'
            {:node node
             :view view
             :f f
             :args args})
    (add-watch view ::render
               (fn [_ _ _ view]
                 (let [node (dom/patchOuter node #(process view path))]
                   (vswap! tree assoc-in (conj path' :node) node))))
    node))

(defn component [f args meta path]
  (let [{:keys [node view] f' :f args' :args}
        (get-in @tree (interpose :children path))]
    (if (and node (= f f'))
      (if (= args @args')
        node
        (rx/no-rx ;; FIXME don't schedule rerendering on this view change
         (reset! args' args)
         (process @view path)))
      (make-component f args meta path))))

(defn mount [elem view]
  (dom/patch elem #(process view [])))

;;; Render batching

(def schedule
  (or (and (exists? js/window)
           (or js/window.requestAnimationFrame
               js/window.webkitRequestAnimationFrame
               js/window.mozRequestAnimationFrame
               js/window.msRequestAnimationFrame
               js/window.oRequestAnimationFrame))
      #(js/setTimeout % 16)))

(def empty-queue {})
(def render-queue (volatile! empty-queue))

(defn flatten-queue [m]
  (->> m
       (tree-seq map? vals)
       (remove map?)))

(defn get-if-map [m k]
  (if (map? m)
    (get m k)
    (reduced m)))

(defn get-in* [m path]
  (reduce get-if-map m path))

(defn assoc-in* [m path x]
  (let [y (get-in* m path)]
    (if (or (nil? y) (map? y))
      (assoc-in m path x)
      m)))

(defn put-into-queue [m [path :as x]]
  (assoc-in* m path x))

(defn put-into-queue! [x]
  (vswap! render-queue put-into-queue x))

(defn put-all-into-queue! [xs]
  (vswap! render-queue reduce put-into-queue xs))

(defn render []
  (let [t (system-time)
        queue (flatten-queue @render-queue)]
    (vreset! render-queue empty-queue)
    (loop [queue queue]
      (when-let [[_ c] (first queue)]
        (if (< (- (system-time) t) 16)
          (do
            (.forceUpdate c)
            (recur (rest queue)))
          (do
            (put-all-into-queue! queue)
            (schedule render)))))))

(defn request-render [path c]
  (when (empty? @render-queue)
    (schedule render))
  (put-into-queue! [path c]))
