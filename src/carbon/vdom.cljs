(ns carbon.vdom
  (:require [cljsjs.inferno]
            [cljsjs.inferno.hyperscript]
            [cljsjs.inferno.create-class]
            [goog.object :as obj]
            [cuerdas.core :as str]
            [carbon.rx :as rx :include-macros true]
            [cljs.test :refer-macros [is]]))

(def ^:dynamic *path* [])

(def id-gen (volatile! 0))

(defn next-id []
  (vswap! id-gen inc))

(def functional-meta
  [:on-component-will-mount
   :on-component-did-mount ; domNode
   :on-component-should-update ; lastProps, nextProps
   :on-component-will-update ; lastProps, nextProps
   :on-component-did-update ; lastProps, nextProps
   :on-component-will-unmount
   :key])

(def component-lifecycle
  [:component-will-mount
   :component-did-mount
   :component-should-update
   :component-will-receive-props
   :component-will-update
   :component-did-update
   :component-will-unmount
   :component-did-unmount])

(def component-meta (conj component-lifecycle :key))

(defn map-keys [f m]
  (persistent! (reduce-kv (fn [m k v] (assoc! m (f k) v)) (transient {}) m)))

(defn deref-if-rx [x]
  (if (satisfies? rx/IReactiveSource x) @x x))

(defn flatten-children [children]
  (->> children
       (tree-seq seq? seq)
       (remove seq?)
       (remove nil?)))

(defn map->js [x]
  (let [y (js-obj)]
    (doseq [[k v] x :when (some? v) :let [v (deref-if-rx v)]]
      (aset y (name k) (if (map? v) (map->js v) v)))
    y))

(def attr-aliases {:class :className})

(defn dealias [kmap map]
  (reduce-kv
   (fn [m old new]
     (if-some [old (get map old)]
       (assoc m new
              (if-some [new (get map new)]
                (cond
                  (string? new) (str new " " old)
                  :else (into new old))
                old))
       m))
   (apply dissoc map (keys kmap)) kmap))

(defn valid-tag? [tag]
  (or (keyword? tag) (string? tag)))

(defn node [tag attrs children]
  {:pre  [(is (valid-tag? tag))
          (is (map? attrs))
          (is (coll? children))]}
  (js/Inferno.h
   (name tag)
   (->> attrs
        (dealias attr-aliases)
        (map-keys str/camel)
        map->js)
   (apply array children)))

(defn parse-arg [[tag & [attrs & children :as args]]]
  (let [full? (map? attrs)]
    [tag
     (if full? attrs {})
     (if full? children args)]))

(declare component request-render)

(defn process [arg]
  (cond
    (vector? arg)
    (let [tag (get arg 0)]
      (if (valid-tag? tag)
        (let [[tag attrs children] (parse-arg arg)]
          (->> children flatten-children (map process) (node tag attrs)))
        (component tag (subvec arg 1) (meta arg))))

    (seq? arg)
    (->> arg flatten-children (map process) (node :div {}))

    :else
    (str arg)))

(defn call-some
  ([this k]
   (when-let [f (get (obj/getValueByKeys this "props" "meta") k)]
     (f this)))
  ([this k args]
   (when-let [f (get (obj/getValueByKeys this "props" "meta") k)]
     (apply f this args))))

(defn lifecycle [k]
  (fn [& args] (this-as this (call-some this k args))))

(def wrapper
  (js/Inferno.createClass
   #js {:displayName
        "CarbonWrapper"

        :componentWillMount
        (fn []
          (this-as this
                   (call-some this :component-will-mount)
                   (add-watch (obj/getValueByKeys this "props" "component")
                              ::render
                              #(request-render
                                (obj/getValueByKeys this "props" "path")
                                this))))

        :componentDidMount
        (lifecycle :component-did-mount)

        :componentShouldUpdate
        (lifecycle :component-should-update)

        :componentWillReceiveProps
        (lifecycle :component-will-receive-props)

        :componentWillUpdate
        (lifecycle :component-will-update)

        :componentDidUpdate
        (lifecycle :component-did-update)

        :componentWillUnmount
        #(this-as this
                  (remove-watch (obj/getValueByKeys this "props" "component") ::render)
                  (call-some this :component-will-unmount))

        :componentDidUnmount
        (lifecycle :component-did-unmount)

        :render
        #(this-as this
                  (rx/no-rx
                   (binding [*path* (obj/getValueByKeys this "props" "path")]
                     (process @(obj/getValueByKeys this "props" "component")))))}))

(defn component* [f args]
  (let [view (rx/no-rx (apply f args))
        f (if (fn? view) view f)]
    (rx/rx (apply f args))))

(defn component [f args meta]
  (js/Inferno.h wrapper
                #js {:component (component* f args)
                     :key (get meta :key)
                     :meta meta
                     :path (conj *path* (next-id))}))

(defn mount [view elem]
  (js/Inferno.render (process view) elem))

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
      (when-let [[path c] (first queue)]
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
