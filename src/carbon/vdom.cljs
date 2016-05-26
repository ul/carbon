(ns carbon.vdom
  (:require js.vdom
            clojure.data
            [carbon.rx :as rx :include-macros true]
            [carbon.vdom.hook :refer [hook]]
            [carbon.vdom.widget :refer [widget]]
            [cljs.test :refer-macros [is]]))

(def diff js/VDOM.diff)
(def patch js/VDOM.patch)
(def create js/VDOM.create)
(def html js/VDOM.h)
(def svg js/VDOM.svg)
(def delegator (js/VDOM.Delegator.))

(defn flatten-children [children]
  (->> children
       (tree-seq seq? seq)
       (remove seq?)
       (remove nil?)))

(defn text-node [s]
  (js/VDOM.VText. (str s)))

(def empty-node (text-node nil))

(defn map->js [x]
  (let [y (js-obj)]
    (doseq [[k v] x :when (some? v)]
      (aset y (name k) (if (map? v) (map->js v) v)))
    y))

(defn node [f tag attrs children]
  {:pre  [(is (#{svg html} f))
          (is (or (keyword? tag) (string? tag)))
          (is (map? attrs))
          (is (coll? children))]
   :post [(is (instance? js/VDOM.VNode %))]}
  (f (name tag) (map->js attrs) (apply array children)))

(declare svg-tree component)

(defn remove-children [elem]
  (loop []
    (when-let [c (.-firstChild elem)]
      (.removeChild elem c)
      (recur))))

(defn parse-arg [[tag & [attrs & children :as args] :as form]]
  (let [full? (map? attrs)]
    [tag
     (let [attrs (if full? attrs {})]
       (if-let [key (-> form meta :key)]
         (assoc attrs :key key)
         attrs))
     (if full? children args)]))

(defn html-tree [arg]
  (cond
    (vector? arg)
    (let [tag (get arg 0)]
      (if (fn? tag)
        (component html-tree tag (subvec arg 1) (-> arg meta :key))
        (let [[tag attrs children] (parse-arg arg)]
          (if (= :svg tag)
            (node svg tag attrs (map svg-tree (flatten-children children)))
            (node html tag attrs (map html-tree (flatten-children children)))))))

    (seq? arg)
    (node html :div {} (map html-tree (flatten-children arg)))

    :else
    (text-node arg)))

(defn svg-tree [arg]
  (cond
    (vector? arg)
    (let [tag (get arg 0)]
      (if (fn? tag)
        (component svg-tree tag (subvec arg 1) (-> arg meta :key))
        (let [[tag attrs children] (parse-arg arg)]
          (node svg tag attrs
                (map (if (= :foreignObject tag) html-tree svg-tree) (flatten-children children))))))

    (seq? arg)
    (node svg :g {} (map svg-tree (flatten-children arg)))

    :else
    (text-node arg)))

;;; Render batching

(def schedule
  (or (and (exists? js/window)
           (or js/window.requestAnimationFrame
               js/window.webkitRequestAnimationFrame
               js/window.mozRequestAnimationFrame
               js/window.msRequestAnimationFrame
               js/window.oRequestAnimationFrame))
      #(js/setTimeout % 16)))

(defn compare-by [keyfn]
  (fn [x y]
    (compare (keyfn x) (keyfn y))))

;(def empty-queue (sorted-set-by (compare-by rank-hash)))
(def empty-queue #{})
(def render-queue (volatile! empty-queue))

(defn render []
  (let [queue @render-queue]
    (vreset! render-queue empty-queue)
    (doseq [f queue]
      (f))))

(defn request-render [component]
  (when (empty? @render-queue)
    (schedule render))
  (vswap! render-queue conj component))

(defn renderer []
  (let [tree (volatile! empty-node)
        root (volatile! (create @tree))]
    (fn [new-tree]
      (let [patches (diff @tree new-tree)]
        (vreset! tree new-tree)
        (vswap! root patch patches)))))

;;; Components

(defn render-component [_ rx-view _ view]
  ((-> rx-view meta (get :render)) view))

(defn init-component [this]
  (let [[t f xs] (get @this :args)
        render (renderer)
        view (apply f xs)
        form-2? (fn? view)
        xs (rx/cell xs)
        rx-view (rx/rx* (if form-2?
                          #(t (apply view @xs))
                          #(t (apply f @xs)))
                        nil
                        {:render render})]
    (swap! this assoc :render render :view rx-view :xs xs)
    (add-watch rx-view :render render-component)
    (render (if form-2? @rx-view (t view)))))

(defn update-component [this prev node]
  (let [[t0 f0 xs0] (:args @prev)
        [t1 f1 xs1] (:args @this)]
    (if (and (= t0 t1) (= f0 f1))
      (let [{:keys [render view xs]} @prev]
        (swap! this assoc :render render :view view :xs xs)
        (reset! xs xs1)
        nil                                                 ; nil-return is important to keep previous node
        )
      (do
        (.destroy prev)
        (.init this)))))

(defn destroy-component [this node]
  (let [{:keys [render view]} @this]
    (remove-watch view :render)
    (render empty-node)))

(defn component [t f xs key]
  (doto
    (widget init-component update-component destroy-component {:args [t f xs]})
    (aset "key" key)))

(defn unmount [elem]
  (when-let [r (aget elem "__carbon_renderer")]
    (r empty-node))
  (aset elem "__carbon_renderer" nil))

(defn mount [elem view]
  (rx/dosync
    (unmount elem)
    (let [r (renderer)]
      (aset elem "__carbon_renderer" r)
      (.appendChild elem (r (html-tree view))))))