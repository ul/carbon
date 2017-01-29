(ns counter.core
  (:require [carbon.vdom :as vdom]
            [carbon.rx :as rx :include-macros true]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (rx/cell [0 0]))

(defn counter [i]
  [:div
   [:button.pure-button {:on-click #(swap! app-state update i inc)} "Inc"]
   [:.counter (get @app-state i)]
   [:button.pure-button {:on-click #(swap! app-state update i dec)} "Dec"]])

(defn app []
  (for [i (-> @app-state count range)]
    ^{:key i} [counter i]))

(vdom/mount [app] (js/document.getElementById "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
