(ns carbon.vdom-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [carbon.vdom :as vdom]
            [clojure.string :as str]
            [goog.object :as obj]
            ["inferno" :refer [Fragment]]))

;; ===========================================================================
;; Pure function tests
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; camel
;; ---------------------------------------------------------------------------

(deftest camel-converts-kebab-case
  (testing "converts kebab-case to camelCase"
    (is (= "onClick" (vdom/camel "on-click")))
    (is (= "fontSize" (vdom/camel "font-size")))
    (is (= "backgroundColor" (vdom/camel "background-color")))
    (is (= "borderTopWidth" (vdom/camel "border-top-width")))))

(deftest camel-leaves-non-kebab
  (testing "leaves strings without hyphens unchanged"
    (is (= "onclick" (vdom/camel "onclick")))
    (is (= "class" (vdom/camel "class")))
    (is (= "id" (vdom/camel "id")))))

(deftest camel-single-char-segments
  (testing "handles single-character segments"
    (is (= "aB" (vdom/camel "a-b")))
    (is (= "xYZ" (vdom/camel "x-y-z")))))

;; ---------------------------------------------------------------------------
;; camel-event-handlers
;; ---------------------------------------------------------------------------

(deftest camel-event-handlers-on-prefixed
  (testing "converts on- prefixed handlers to camelCase"
    (is (= "onClick" (vdom/camel-event-handlers :on-click)))
    (is (= "onMouseDown" (vdom/camel-event-handlers :on-mouse-down)))
    (is (= "onChange" (vdom/camel-event-handlers :on-change)))
    (is (= "onKeyDown" (vdom/camel-event-handlers :on-key-down)))
    (is (= "onDoubleClick" (vdom/camel-event-handlers :on-double-click)))))

(deftest camel-event-handlers-non-on-prefixed
  (testing "leaves non-on- keywords unchanged"
    (is (= "class" (vdom/camel-event-handlers :class)))
    (is (= "id" (vdom/camel-event-handlers :id)))
    (is (= "style" (vdom/camel-event-handlers :style)))
    (is (= "data-value" (vdom/camel-event-handlers :data-value)))
    (is (= "href" (vdom/camel-event-handlers :href)))
    (is (= "className" (vdom/camel-event-handlers :className)))))

;; ---------------------------------------------------------------------------
;; map-keys
;; ---------------------------------------------------------------------------

(deftest map-keys-transforms-keys
  (testing "transforms all keys with function"
    (is (= {"a" 1, "b" 2} (vdom/map-keys name {:a 1, :b 2})))))

(deftest map-keys-empty-map
  (testing "empty map returns empty map" (is (= {} (vdom/map-keys name {})))))

(deftest map-keys-preserves-values
  (testing "values are not modified"
    (is (= {"x" [1 2 3]} (vdom/map-keys name {:x [1 2 3]})))
    (is (= {"a" nil} (vdom/map-keys name {:a nil})))))

(deftest map-keys-with-string-transform
  (testing "works with string manipulation"
    (is (= {"A" 1, "B" 2}
           (vdom/map-keys #(str/upper-case (name %)) {:a 1, :b 2})))))

;; ---------------------------------------------------------------------------
;; filter-vals
;; ---------------------------------------------------------------------------

(deftest filter-vals-removes-matching
  (testing "removes entries where predicate returns falsy for value"
    (is (= {:a 1, :b 2} (vdom/filter-vals some? {:a 1, :b 2, :c nil})))))

(deftest filter-vals-empty-map
  (testing "empty map returns empty map"
    (is (= {} (vdom/filter-vals some? {})))))

(deftest filter-vals-all-filtered
  (testing "all values filtered returns empty map"
    (is (= {} (vdom/filter-vals some? {:a nil, :b nil})))))

(deftest filter-vals-none-filtered
  (testing "no values filtered returns same map"
    (is (= {:a 1, :b 2} (vdom/filter-vals some? {:a 1, :b 2})))))

(deftest filter-vals-custom-predicate
  (testing "works with custom predicate"
    (is (= {:a 2, :b 4} (vdom/filter-vals even? {:a 2, :b 4, :c 3})))))

;; ---------------------------------------------------------------------------
;; flatten-children
;; ---------------------------------------------------------------------------

(deftest flatten-children-flat-list
  (testing "flat list passes through"
    (is (= [1 2 3] (vec (vdom/flatten-children '(1 2 3)))))))

(deftest flatten-children-removes-nils
  (testing "nils are removed"
    (is (= [1 2 3] (vec (vdom/flatten-children '(1 nil 2 nil 3)))))))

(deftest flatten-children-deeply-nested
  (testing "deeply nested lists are flattened"
    (is (= [1 2 3] (vec (vdom/flatten-children '((1 (2 (3))))))))))

(deftest flatten-children-preserves-vectors
  (testing "vectors are treated as leaf nodes (not flattened)"
    (is (= [[:div] [:span]] (vec (vdom/flatten-children '([:div] [:span])))))))

(deftest flatten-children-empty
  (testing "empty input returns empty"
    (is (= [] (vec (vdom/flatten-children '()))))))

(deftest flatten-children-nested-with-nils
  (testing "nested lists with nils are flattened and cleaned"
    (is (= [1 2] (vec (vdom/flatten-children '(nil (1 nil (nil 2)) nil)))))))

(deftest flatten-children-mixed-types
  (testing "preserves various non-seq types"
    (is (= ["a" 1 :kw [:vec]]
           (vec (vdom/flatten-children '("a" 1 :kw [:vec])))))))

;; ---------------------------------------------------------------------------
;; valid-tag?
;; ---------------------------------------------------------------------------

(deftest valid-tag-keywords
  (testing "keywords are valid tags"
    (is (true? (vdom/valid-tag? :div)))
    (is (true? (vdom/valid-tag? :span)))
    (is (true? (vdom/valid-tag? :my-component)))))

(deftest valid-tag-strings
  (testing "strings are valid tags"
    (is (true? (vdom/valid-tag? "div")))
    (is (true? (vdom/valid-tag? "custom-element")))))

(deftest valid-tag-invalid-types
  (testing "other types are invalid"
    (is (not (vdom/valid-tag? 42)))
    (is (not (vdom/valid-tag? nil)))
    (is (not (vdom/valid-tag? identity)))
    (is (not (vdom/valid-tag? {:a 1})))))

;; ---------------------------------------------------------------------------
;; attrs->js
;; ---------------------------------------------------------------------------

(deftest attrs->js-maps-class-to-classname
  (testing "maps :class to className"
    (let [result (vdom/attrs->js {:class "foo"})]
      (is (= "foo" (obj/get result "className"))))))

(deftest attrs->js-no-op-without-class
  (testing "passes through attrs without :class"
    (let [result (vdom/attrs->js {:id "foo"})]
      (is (= "foo" (obj/get result "id"))))))

(deftest attrs->js-empty-map
  (testing "empty map produces empty object"
    (let [result (vdom/attrs->js {})]
      (is (= 0 (.-length (js/Object.keys result)))))))

(deftest attrs->js-preserves-other-attrs
  (testing "other attributes are preserved alongside className"
    (let [result (vdom/attrs->js
                   {:class "foo", :id "bar", :style {:color "red"}})]
      (is (= "foo" (obj/get result "className")))
      (is (= "bar" (obj/get result "id")))
      (is (= "red" (obj/get (obj/get result "style") "color"))))))

;; ---------------------------------------------------------------------------
;; parse-arg
;; ---------------------------------------------------------------------------

(deftest parse-arg-with-attrs-and-children
  (testing "tag with map attrs and children"
    (let [[tag attrs children] (vdom/parse-arg [:div {:class "foo"} "hello"])]
      (is (= :div tag))
      (is (= {:class "foo"} attrs))
      (is (= '("hello") children)))))

(deftest parse-arg-without-attrs
  (testing "tag without attrs treats first arg as child"
    (let [[tag attrs children] (vdom/parse-arg [:div "hello" "world"])]
      (is (= :div tag))
      (is (= {} attrs))
      (is (= '("hello" "world") children)))))

(deftest parse-arg-tag-only
  (testing "tag only returns empty attrs and nil children"
    (let [[tag attrs children] (vdom/parse-arg [:div])]
      (is (= :div tag))
      (is (= {} attrs))
      (is (nil? children)))))

(deftest parse-arg-empty-attrs
  (testing "empty map attrs with children"
    (let [[tag attrs children] (vdom/parse-arg [:div {} "child"])]
      (is (= :div tag))
      (is (= {} attrs))
      (is (= '("child") children)))))

(deftest parse-arg-attrs-no-children
  (testing "attrs with no children"
    (let [[tag attrs children] (vdom/parse-arg [:div {:id "x"}])]
      (is (= :div tag))
      (is (= {:id "x"} attrs))
      (is (nil? children)))))

(deftest parse-arg-multiple-children
  (testing "multiple children are collected"
    (let [[tag attrs children] (vdom/parse-arg [:ul {:class "list"} "a" "b"
                                                "c"])]
      (is (= :ul tag))
      (is (= {:class "list"} attrs))
      (is (= '("a" "b" "c") children)))))

;; ---------------------------------------------------------------------------
;; next-id
;; ---------------------------------------------------------------------------

(deftest next-id-increments
  (testing "generates strictly incrementing IDs"
    (let [id1 (vdom/next-id)
          id2 (vdom/next-id)
          id3 (vdom/next-id)]
      (is (< id1 id2))
      (is (< id2 id3))
      (is (= 1 (- id2 id1)))
      (is (= 1 (- id3 id2))))))

;; ===========================================================================
;; VNode creation tests (require Inferno, no DOM needed)
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; node
;; ---------------------------------------------------------------------------

(deftest node-creates-vnode-with-type
  (testing "creates VNode with correct tag type"
    (let [vnode (vdom/node :div {} (array))] (is (= "div" (.-type vnode))))))

(deftest node-creates-vnode-with-classname
  (testing "creates VNode with className from :class alias"
    (let [vnode (vdom/node :div {:class "foo"} (array))]
      (is (= "foo" (.-className vnode))))))

(deftest node-creates-vnode-with-children
  (testing "creates VNode with string child wrapped as text VNode"
    (let [vnode (vdom/node :div {} (array "hello"))
          children (.-children vnode)]
      ;; Inferno wraps strings in text VNodes (flags=16, .-children is the
      ;; text)
      (is (array? children))
      (is (= 1 (.-length children)))
      (is (= "hello" (.-children (aget children 0)))))))

(deftest node-filters-nil-attrs
  (testing "nil attribute values are filtered out"
    (let [vnode (vdom/node :div {:id "x", :title nil} (array))]
      (is (= "x" (.. vnode -props -id))))))

(deftest node-converts-event-handlers
  (testing "on- prefixed handlers are converted to camelCase"
    (let [handler (fn [])
          vnode (vdom/node :div {:on-click handler} (array))]
      (is (= handler (.. vnode -props -onClick))))))

(deftest node-different-tags
  (testing "various HTML tags"
    (is (= "span" (.-type (vdom/node :span {} (array)))))
    (is (= "ul" (.-type (vdom/node :ul {} (array)))))
    (is (= "input" (.-type (vdom/node :input {} (array)))))
    (is (= "a" (.-type (vdom/node :a {} (array)))))))

(deftest node-multiple-children
  (testing "multiple children as array of text VNodes"
    (let [vnode (vdom/node :div {} (array "a" "b" "c"))
          children (.-children vnode)]
      (is (array? children))
      (is (= 3 (.-length children)))
      (is (= "a" (.-children (aget children 0))))
      (is (= "b" (.-children (aget children 1))))
      (is (= "c" (.-children (aget children 2)))))))

;; ---------------------------------------------------------------------------
;; process
;; ---------------------------------------------------------------------------

(deftest process-keyword-tag
  (testing "processes keyword tag vector"
    (let [vnode (vdom/process [:div])] (is (= "div" (.-type vnode))))))

(deftest process-tag-with-attrs
  (testing "processes tag with attributes"
    (let [vnode (vdom/process [:div {:class "foo"}])]
      (is (= "div" (.-type vnode)))
      (is (= "foo" (.-className vnode))))))

(deftest process-nested-elements
  (testing "processes nested elements with child type preserved"
    (let [vnode (vdom/process [:div [:span "hello"]])
          children (.-children vnode)]
      (is (= "div" (.-type vnode)))
      (is (array? children))
      (is (= 1 (.-length children)))
      (is (= "span" (.-type (aget children 0)))))))

(deftest process-string-tag
  (testing "processes string tag"
    (let [vnode (vdom/process ["div"])] (is (= "div" (.-type vnode))))))

(deftest process-text-conversion
  (testing "converts non-element values to strings"
    (is (= "42" (vdom/process 42)))
    (is (= "hello" (vdom/process "hello")))
    (is (= "true" (vdom/process true)))))

(deftest process-sequence-as-fragment
  (testing "processes sequences as Fragment with correct children"
    (let [vnode (vdom/process '([:div "a"] [:div "b"]))
          children (.-children vnode)]
      ;; Inferno encodes Fragment type as VNodeFlags bitmask (8192), not
      ;; the. Fragment symbol string "$F".
      (is (= 8192 (.-type vnode)))
      (is (array? children))
      (is (= 2 (.-length children)))
      (is (= "div" (.-type (aget children 0))))
      (is (= "div" (.-type (aget children 1)))))))

(deftest process-multiple-children
  (testing "processes tag with multiple children preserving count and types"
    (let [vnode (vdom/process [:ul [:li "a"] [:li "b"] [:li "c"]])
          children (.-children vnode)]
      (is (= "ul" (.-type vnode)))
      (is (array? children))
      (is (= 3 (.-length children)))
      (is (= "li" (.-type (aget children 0))))
      (is (= "li" (.-type (aget children 1))))
      (is (= "li" (.-type (aget children 2)))))))

(deftest process-nested-sequence-children
  (testing "handles list children (e.g. from for) flattened into parent"
    (let [vnode (vdom/process [:div '([:span "a"] [:span "b"])])
          children (.-children vnode)]
      (is (= "div" (.-type vnode)))
      (is (array? children))
      (is (= 2 (.-length children)))
      (is (= "span" (.-type (aget children 0))))
      (is (= "span" (.-type (aget children 1)))))))

(deftest process-nil-keyword
  (testing "processes :keyword to string"
    (is (= ":keyword" (vdom/process :keyword)))))

(deftest process-component-function
  (testing "function tags produce component VNodes with wrapper type"
    (let [my-comp (fn [] [:div "hello"])
          vnode (vdom/process [my-comp])]
      ;; component returns an h() call with a wrapper class constructor
      (is (fn? (.-type vnode)))
      ;; The wrapper should be the cached wrapper for this function
      (is (identical? (vdom/get-wrapper my-comp) (.-type vnode))))))

(deftest process-component-with-args
  (testing "function tags receive args via props"
    (let [my-comp (fn [text] [:span text])
          vnode (vdom/process [my-comp "hello"])]
      (is (some? vnode))
      (is (= ["hello"]
             (js->clj (-> (.-props ^js vnode)
                          (.-args))))))))

(deftest process-component-with-key
  (testing "meta :key is passed to component"
    (let [my-comp (fn [] [:div])
          vnode (vdom/process ^{:key "k1"} [my-comp])]
      (is (= "k1" (.-key vnode))))))

;; ===========================================================================
;; DOM integration tests (require jsdom)
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; mount
;; ---------------------------------------------------------------------------

(deftest mount-simple-element
  (testing "mounts a simple element to DOM"
    (let [elem (js/document.createElement "div")]
      (vdom/mount [:div {:class "test"} "Hello"] elem)
      (let [child (.-firstChild elem)]
        (is (= "DIV" (.-tagName child)))
        (is (= "test" (.-className child)))
        (is (= "Hello" (.-textContent child)))))))

(deftest mount-nested-elements
  (testing "mounts nested elements"
    (let [elem (js/document.createElement "div")]
      (vdom/mount [:div [:span "inner"]] elem)
      (let [span (.. elem -firstChild -firstChild)]
        (is (= "SPAN" (.-tagName span)))
        (is (= "inner" (.-textContent span)))))))

(deftest mount-with-attributes
  (testing "mounts element with various attributes"
    (let [elem (js/document.createElement "div")]
      (vdom/mount [:a {:href "/test", :id "link1"} "Link"] elem)
      (let [a (.-firstChild elem)]
        (is (= "A" (.-tagName a)))
        (is (= "/test" (.getAttribute a "href")))
        (is (= "link1" (.-id a)))
        (is (= "Link" (.-textContent a)))))))

(deftest mount-list-of-elements
  (testing "mounts a sequence as fragment with all children rendered"
    (let [elem (js/document.createElement "div")]
      (vdom/mount '([:div "a"] [:div "b"]) elem)
      (is (= "ab" (.-textContent elem)))
      ;; Fragment renders children directly into the container
      (is (= 2 (.. elem -childNodes -length)))
      (is (= "DIV" (.. elem -firstChild -tagName)))
      (is (= "a" (.. elem -firstChild -textContent)))
      (is (= "DIV" (.. elem -lastChild -tagName)))
      (is (= "b" (.. elem -lastChild -textContent))))))

(deftest mount-replaces-content
  (testing "subsequent mount replaces previous content"
    (let [elem (js/document.createElement "div")]
      (vdom/mount [:div "first"] elem)
      (is (= "first" (.-textContent elem)))
      (vdom/mount [:div "second"] elem)
      (is (= "second" (.-textContent elem))))))

(deftest mount-empty-div
  (testing "mounts empty div"
    (let [elem (js/document.createElement "div")]
      (vdom/mount [:div] elem)
      (is (= "DIV" (.. elem -firstChild -tagName)))
      (is (= "" (.-textContent (.-firstChild elem)))))))

;; ---------------------------------------------------------------------------
;; Render batching
;; ---------------------------------------------------------------------------

(deftest render-queue-starts-empty
  (testing "render queue is empty initially (after any prior renders)"
    (vdom/render)
    (is (empty? @vdom/render-queue))))

(deftest clear-render-removes-path
  (testing "clear-render removes a path from queue"
    (let [path [:test :path]]
      (vswap! vdom/render-queue assoc path "mock-component")
      (vdom/clear-render path)
      (is (not (contains? @vdom/render-queue path))))))

(deftest request-render-adds-to-queue
  (testing "request-render adds component to queue"
    ;; Clear queue first
    (vreset! vdom/render-queue vdom/empty-queue)
    (let [path [:test :component]
          mock-component #js {:forceUpdate (fn [])}]
      (vdom/request-render path mock-component)
      (is (contains? @vdom/render-queue path))
      ;; Clean up
      (vreset! vdom/render-queue vdom/empty-queue))))

(deftest render-clears-queue-and-updates
  (testing "render calls forceUpdate on queued components and clears queue"
    (vreset! vdom/render-queue vdom/empty-queue)
    (let [updated (atom false)
          mock-component #js {:forceUpdate (fn [] (reset! updated true))}
          path [:test :render]]
      (vswap! vdom/render-queue assoc path mock-component)
      (vdom/render)
      (is (true? @updated))
      (is (empty? @vdom/render-queue)))))

(deftest render-updates-multiple-components
  (testing "render updates all queued components"
    (vreset! vdom/render-queue vdom/empty-queue)
    (let [updates (atom [])
          comp-a #js {:forceUpdate (fn [] (swap! updates conj :a))}
          comp-b #js {:forceUpdate (fn [] (swap! updates conj :b))}]
      (vswap! vdom/render-queue assoc [:a] comp-a)
      (vswap! vdom/render-queue assoc [:b] comp-b)
      (vdom/render)
      (is (= 2 (count @updates)))
      (is (empty? @vdom/render-queue)))))

;; ---------------------------------------------------------------------------
;; Wrapper cache
;; ---------------------------------------------------------------------------

(deftest get-wrapper-caches
  (testing "get-wrapper returns same wrapper for same function"
    (let [f (fn [] [:div])
          w1 (vdom/get-wrapper f)
          w2 (vdom/get-wrapper f)]
      (is (identical? w1 w2)))))

(deftest get-wrapper-different-for-different-fns
  (testing "get-wrapper returns different wrappers for different functions"
    (let [f1 (fn [] [:div])
          f2 (fn [] [:span])
          w1 (vdom/get-wrapper f1)
          w2 (vdom/get-wrapper f2)]
      (is (not (identical? w1 w2))))))
