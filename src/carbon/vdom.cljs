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

(defn map->js [x]
  (let [y (js-obj)]
    (doseq [[k v] x]
      (aset y (name k) v))
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

(defn html-tree [arg]
  (cond
    (vector? arg)
    (let [[tag attrs & children] arg]
      (assert (map? attrs))
      (cond
        (fn? tag)
        (component html-tree tag children (:key attrs))

        (= :svg tag)
        (node svg tag attrs (map svg-tree (flatten-children children)))

        :else
        (node html tag attrs (map html-tree (flatten-children children)))))

    (seq? arg)
    (node html :div {} (map html-tree (flatten-children arg)))

    :else
    (text-node arg)))

(defn svg-tree [arg]
  (cond
    (vector? arg)
    (let [[tag attrs & children] arg]
      (assert (map? attrs))
      (cond
        (fn? tag)
        (component svg-tree tag children (:key attrs))

        :else
        (node svg tag attrs
              (map (if (= :foreignObject tag) html-tree svg-tree) (flatten-children children)))))

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

(defn renderer [tree-builder]
  (let [tree (volatile! (text-node nil))
        root (volatile! (create @tree))]
    (fn [view]
      (let [new-tree (tree-builder view)
            patches (diff @tree new-tree)]
        (vreset! tree new-tree)
        (vswap! root patch patches)))))

;;; Components

(defn init-component [this]
  (let [[t f xs] (:args @this)
        render (renderer t)
        initital-view (apply f xs)
        form-2? (fn? initital-view)
        view (if form-2?
               (rx/rx (apply initital-view xs))
               (rx/rx (apply f xs)))
        update #(render @view)]
    (swap! this assoc :view view)
    (add-watch view :render #(request-render update))
    (if form-2?
      (update)
      (render initital-view))))

(defn update-component [this prev node]
  (if (= (:args @this) (:args @prev))
    (do
      (swap! this assoc :view (:view @prev))
      nil)
    (do
      (.destroy prev)
      (.init this))))

(defn destroy-component [this node]
  (remove-watch (:view @this) :render))

(defn component [t f xs key]
  (doto
    (widget init-component update-component destroy-component {:args [t f xs]})
    (aset "key" key)))

(defn mount [elem view]
  (let [r (renderer html-tree)]
    (.appendChild elem (r view))))

(defn unmount [elem]
  (remove-children elem))