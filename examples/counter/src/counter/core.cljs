(ns counter.core
  (:require [carbon.vdom :as vdom]
            [carbon.rx :as rx :include-macros true]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (rx/cell [0 0]))
(def cs [(rx/cursor app-state [0]) (rx/cursor app-state [1])])

(defn counter [c]
  [:div
   [:button.pure-button {:onclick #(swap! c inc)} "Inc"]
   [:.counter @c]
   [:button.pure-button {:onclick #(swap! c dec)} "Dec"]])

(defn ppp []
  [:div (pr-str @app-state)
   (for [i (-> @app-state count range)]
     ^{:key i} [counter (get cs i)])])

(defn app []
  [:div
   [ppp]
   ])

(vdom/mount (js/document.getElementById "app") [app])

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
