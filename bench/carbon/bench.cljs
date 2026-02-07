(ns carbon.bench
  (:require ["tinybench" :refer [Bench]]
            [carbon.vdom :as vdom]))

;; ---- Fixtures ----

(def flat-3 (list "a" "b" "c"))
(def flat-10 (apply list (repeat 10 "x")))
(def nested-3 (list "a" (list "b" (list "c"))))
(def nested-10 (list "a" (list "b" "c" (list "d" "e" (list "f" "g" (list "h" "i" "j"))))))
(def mixed-20 (apply list (interleave (repeat 10 "x") (map #(list (str %)) (range 10)))))

(def small-attrs {:class "btn" :on-click identity})
(def medium-attrs {:class "card" :id "main" :data-value "42"
                   :on-click identity :on-mouse-enter identity
                   :style "color:red" :title "hello"})
(def large-attrs (into {} (map (fn [i] [(keyword (str "data-k" i)) (str "v" i)]) (range 20))))

(defn small-view []
  [:div {:class "app"}
   [:span "hello"]
   [:button {:on-click identity} "click"]])

(defn medium-view []
  [:div {:class "container"}
   [:h1 "Title"]
   [:p "Description"]
   [:ul
    (for [i (range 10)]
      ^{:key i} [:li {:class "item"} (str "Item " i)])]
   [:button {:class "btn" :on-click identity} "OK"]])

(defn large-view []
  [:div {:class "root"}
   [:header {:class "hd"} [:h1 "Title"]]
   [:main
    [:table
     [:thead [:tr [:th "A"] [:th "B"] [:th "C"]]]
     [:tbody
      (for [r (range 50)]
        ^{:key r} [:tr
                   [:td {:class "cell"} (str "a" r)]
                   [:td {:class "cell"} (str "b" r)]
                   [:td {:class "cell" :on-click identity} (str "c" r)]])]]]
   [:footer [:span "done"]]])

;; ---- Helpers ----

(defn add-bench [^js bench name f]
  (.add bench name f)
  bench)

(defn print-results [^js bench]
  (doseq [^js task (.-tasks bench)]
    (let [name (.-name task)
          lat (-> task .-result .-latency)
          mean (.-mean lat)
          sd (.-sd lat)
          p75 (.-p75 lat)
          p99 (.-p99 lat)
          n (.-samplesCount lat)]
      (println (str "  " name
                    "  mean=" (.toFixed (* mean 1000) 2) " µs"
                    "  sd=" (.toFixed (* sd 1000) 2) " µs"
                    "  p75=" (.toFixed (* p75 1000) 2) " µs"
                    "  p99=" (.toFixed (* p99 1000) 2) " µs"
                    "  (" n " samples)")))))

(defn run-suite [name benchmarks]
  (let [bench (Bench. #js {:warmupIterations 1000})]
    (doseq [[bname f] benchmarks]
      (add-bench bench bname f))
    (-> (.run bench)
        (.then (fn [_]
                 (println name)
                 (print-results bench)
                 (println))))))

;; ---- Runner ----

(defn -main []
  (println "Carbon VDOM Benchmarks")
  (println (str "Node " (.-version js/process)))
  (println)
  (-> (run-suite "flatten-children:"
                 [["flat-3"    (fn [] (vdom/flatten-children flat-3))]
                  ["flat-10"   (fn [] (vdom/flatten-children flat-10))]
                  ["nested-3"  (fn [] (vdom/flatten-children nested-3))]
                  ["nested-10" (fn [] (vdom/flatten-children nested-10))]
                  ["mixed-20"  (fn [] (vdom/flatten-children mixed-20))]])
      (.then (fn [_] (run-suite "node (attrs pipeline):"
                                [["small-attrs (2 keys)"  (fn [] (vdom/node :div small-attrs []))]
                                 ["medium-attrs (7 keys)" (fn [] (vdom/node :div medium-attrs []))]
                                 ["large-attrs (20 keys)" (fn [] (vdom/node :div large-attrs []))]])))
      (.then (fn [_] (run-suite "process (full view):"
                                [["small (3 el)"    (fn [] (vdom/process (small-view)))]
                                 ["medium (~15 el)" (fn [] (vdom/process (medium-view)))]
                                 ["large (~160 el)" (fn [] (vdom/process (large-view)))]])))
      (.then (fn [_] (println "Done.")))))
