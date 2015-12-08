(ns carbon.vdom
  "TODO: rx-attribute hook"
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require js.vdom
            [cljs.core.async :as async]
            [carbon.rx :as rx :include-macros true]
            [carbon.vdom.hook :refer [hook]]))

(def diff js/VDOM.diff)
(def patch js/VDOM.patch)
(def create js/VDOM.create)
(def html js/VDOM.h)
(def svg js/VDOM.svg)
(def d (js/VDOM.Delegator.))

(defn flatten-children [children]
  (->> children
       (tree-seq seq? seq)
       (remove seq?)
       (remove nil?)))

(defn text-node [s]
  (js/VDOM.VText. (str s)))

(defn node [f tag attrs children]
  (f (name tag) (clj->js attrs) (clj->js children)))

(declare svg-tree)

(declare renderer)

(defn remove-children [elem]
  (loop []
    (when-let [c (.-firstChild elem)]
      (.removeChild elem c)
      (recur))))

(defn mount [elem view]
  (let [r (or (.-renderer elem) (renderer elem))]
    (set! (.-renderer elem) r)
    (add-watch view ::mount (fn [_ _ _ view] (r view)))
    (r @view)))

(defn unmount [elem view]
  (when view (remove-watch view ::mount))
  (remove-children elem))

(defn mount-hook [[f & xs]]
  (let [view (volatile! nil)]
    (hook
      (fn [elem _ _]
        (mount elem (vreset! view (rx/rx (apply f xs)))))
      (fn [elem _ next]
        (when-not next
          (unmount elem @view))))))

(defn html-tree [arg]
  (cond
    (vector? arg)
    (let [[tag attrs & children] arg]
      (cond
        (fn? tag)
        (node html :span {:mount (mount-hook arg)} [])

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
      (cond
        (fn? tag)
        (node svg :g {:mount (mount-hook arg)} [])

        :else
        (node svg tag attrs
              (map (if (= :foreignObject tag) html-tree svg-tree) (flatten-children children)))))

    (seq? arg)
    (node svg :g {} (map svg-tree (flatten-children arg)))

    :else
    (text-node arg)))


;;; Render batching


(def raf
  (or
    (.-requestAnimationFrame js/window)
    (.-webkitRequestAnimationFrame js/window)
    (.-mozRequestAnimationFrame js/window)
    (.-msRequestAnimationFrame js/window)
    (.-oRequestAnimationFrame js/window)
    (let [t0 (.getTime (js/Date.))]
      (fn [f]
        (js/setTimeout
          #(f (- (.getTime (js/Date.)) t0))
          16.66666)))))

(defn renderer [elem]
  (let [tree (atom (text-node nil))
        root (atom (create @tree))
        !view (async/chan (async/sliding-buffer 1))
        !raf (async/chan (async/sliding-buffer 1))
        !render (async/map identity [!view !raf] (async/sliding-buffer 1))]
    (.appendChild elem @root)
    (go-loop []
      (when-let [view (async/<! !render)]
        (let [new-tree (html-tree view)
              patches (diff @tree new-tree)]
          (reset! tree new-tree)
          (swap! root patch patches))
        (recur)))
    (fn [view]
      (async/put! !view view)
      (raf #(async/put! !raf true)))))
