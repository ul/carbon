(ns carbon.vdom
  (:require [cljsjs.inferno]
            [cljsjs.inferno.hyperscript]
            [cljsjs.inferno.create-class]
            [goog.object :as obj]
            [carbon.rx :as rx :include-macros true]
            [cljs.test :refer-macros [is]]
            [clojure.string :as str]))

(def kebab-start (js/RegExp. "-(\\w)" "g"))

(defn upper-case-second [x]
  (-> x (aget 1) str/upper-case))

(defn camel [s]
  (-> s name (.replace kebab-start upper-case-second)))

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
   :key
   :ref])

(def component-lifecycle
  [:component-will-mount
   :component-did-mount
   :component-should-update
   :component-will-receive-props
   :component-will-update
   :component-did-update
   :component-will-unmount
   :component-did-unmount])

(def component-meta (conj component-lifecycle :key :ref))

(defn map-keys [f m]
  (persistent! (reduce-kv (fn [m k v] (assoc! m (f k) v)) (transient {}) m)))

(defn filter-vals [p m]
  (reduce-kv (fn [m k v] (if (p v) m (dissoc m k))) m m))

(defn flatten-children [children]
  (->> children
       (tree-seq seq? seq)
       (remove seq?)
       (remove nil?)))

(defn valid-tag? [tag]
  (or (keyword? tag) (string? tag)))

(defn node [tag attrs children]
  {:pre  [(is (valid-tag? tag))
          (is (map? attrs))
          (is (coll? children))]}
  (js/Inferno.h
   (name tag)
   (->> attrs
        (filter-vals some?)
        (map-keys camel)
        clj->js)
   (apply array children)))

(defn parse-arg [[tag & [attrs & children :as args]]]
  (let [full? (map? attrs)]
    [tag
     (if full? attrs {})
     (if full? children args)]))

(declare component request-render clear-render)

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

(defn get-prop [this k]
  (obj/getValueByKeys this "props" (name k)))

(defn get-state [this k]
  (obj/getValueByKeys this "state" (name k)))

(defn call-some
  ([this k]
   (when-let [f (get (get-prop this "meta") k)]
     (f this)))
  ([this k args]
   (when-let [f (get (get-prop this "meta") k)]
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
                   (let [f (get-prop this :f)
                         args (get-prop this :args)
                         path (conj (get-prop this :parent-path) (next-id))
                         view (rx/cell f)
                         args (rx/cell args)
                         component (rx/rx (apply @view @args))
                         form @component]
                     (when (fn? form)
                       (reset! view form))
                     (.setState this #js {:component component
                                          :path path
                                          :f f
                                          :view view
                                          :args args})
                     (add-watch component ::render #(request-render path this)))))

        :componentDidMount
        (lifecycle :component-did-mount)

        :componentShouldUpdate
        (constantly false)

        :componentWillReceiveProps
        (fn [next-props]
          (this-as this
            (let [next-f (obj/get next-props "f")
                  next-args (obj/get next-props "args")
                  f (get-state this :f)
                  args (get-state this :args)]
              (if (= f next-f)
                (reset! args next-args)
                (let [view (get-state this :view)
                      component (get-state this :component)]
                  (rx/dosync
                   (reset! view next-f)
                   (reset! args next-args))
                  (let [form @component]
                    (when (fn? form)
                      (reset! view form))))))))

        :componentWillUpdate
        (lifecycle :component-will-update)

        :componentDidUpdate
        (lifecycle :component-did-update)

        :componentWillUnmount
        #(this-as this
                  (remove-watch (get-state this :component) ::render)
                  (call-some this :component-will-unmount))

        :componentDidUnmount
        (lifecycle :component-did-unmount)

        :render
        #(this-as this
                  (rx/no-rx
                   (binding [*path* (get-state this :path)]
                     (process @(get-state this :component)))))}))

(defn component [f args meta]
  (js/Inferno.h wrapper
                #js {:f f
                     :args args
                     :key (get meta :key)
                     :ref (get meta :ref)
                     :meta meta
                     :parent-path *path*}))

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

(def empty-queue (sorted-map))
(def render-queue (volatile! empty-queue))

(defn render []
  (let [queue @render-queue]
    (vreset! render-queue empty-queue)
    (doseq [c (vals queue)]
      (.forceUpdate c)))
  #_(let [t (system-time)]
    (loop []
      (when-let [[path c] (first @render-queue)]
        (if (< (- (system-time) t) 16)
          (do
            (.forceUpdate c)
            (clear-render path)
            (recur))
          (when-not (empty? @render-queue)
            (schedule render)))))))

(defn request-render [path c]
  (when (empty? @render-queue)
    (schedule render))
  (vswap! render-queue assoc path c))

(defn clear-render [path]
  (vswap! render-queue dissoc path))
