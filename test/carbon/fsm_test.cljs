(ns carbon.fsm-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [carbon.fsm :as fsm]))

;; ---------------------------------------------------------------------------
;; Predicate: on
;; ---------------------------------------------------------------------------

(deftest on-matches-event-type
  (testing "matches event type in first position of event vector"
    (let [pred (fsm/on :click :hover)]
      (is (pred {} [:click]))
      (is (pred {} [:hover]))
      (is (not (pred {} [:drag]))))))

(deftest on-single-event-type
  (testing "works with a single event type"
    (let [pred (fsm/on :click)]
      (is (pred {} [:click "data"]))
      (is (not (pred {} [:hover]))))))

(deftest on-ignores-context
  (testing "context argument is ignored"
    (let [pred (fsm/on :click)]
      (is (pred nil [:click]))
      (is (pred {:any "ctx"} [:click]))
      (is (pred "string-ctx" [:click])))))

;; ---------------------------------------------------------------------------
;; Predicate: with
;; ---------------------------------------------------------------------------

(deftest with-matches-event-data
  (testing "matches event data in second position"
    (let [pred (fsm/with :left :right)]
      (is (pred {} [:click :left]))
      (is (pred {} [:click :right]))
      (is (not (pred {} [:click :middle]))))))

(deftest with-single-data-value
  (testing "works with single data value"
    (let [pred (fsm/with :left)]
      (is (pred {} [:click :left]))
      (is (not (pred {} [:click :right]))))))

(deftest with-ignores-event-type
  (testing "event type in first position is ignored"
    (let [pred (fsm/with :data)]
      (is (pred {} [:click :data]))
      (is (pred {} [:hover :data]))
      (is (pred {} [:any-event :data])))))

;; ---------------------------------------------------------------------------
;; Predicate combinator: and-fn
;; ---------------------------------------------------------------------------

(deftest and-fn-all-must-be-true
  (testing "all predicates must return truthy"
    (let [pred (fsm/and-fn (fsm/on :click) (fsm/with :left))]
      (is (pred {} [:click :left]))
      (is (not (pred {} [:hover :left])))
      (is (not (pred {} [:click :right]))))))

(deftest and-fn-empty-predicates
  (testing "no predicates returns true"
    (let [pred (fsm/and-fn)] (is (true? (pred {} [:anything]))))))

(deftest and-fn-single-predicate
  (testing "single predicate delegates directly"
    (let [pred (fsm/and-fn (fsm/on :click))]
      (is (pred {} [:click]))
      (is (not (pred {} [:hover]))))))

(deftest and-fn-three-predicates
  (testing "short-circuits on first falsy"
    (let [call-count (atom 0)
          counting-pred (fn [& _] (swap! call-count inc) false)
          pred (fsm/and-fn (constantly true) counting-pred (constantly true))]
      (is (not (pred {} [:x])))
      ;; counting-pred was called, but the third pred should not run
      (is (= 1 @call-count)))))

;; ---------------------------------------------------------------------------
;; execute: basic transitions
;; ---------------------------------------------------------------------------

(deftest execute-basic-transition
  (testing "transitions to new state when condition matches"
    (let [spec {:idle {:transitions [[(fsm/on :start) :running]]},
                :running {:transitions []}}]
      (is (= :running (fsm/execute :idle spec {} [:start]))))))

(deftest execute-no-matching-transition
  (testing "stays in current state when no condition matches"
    (let [spec {:idle {:transitions [[(fsm/on :start) :running]]}}]
      (is (= :idle (fsm/execute :idle spec {} [:stop]))))))

(deftest execute-missing-state-spec
  (testing "missing state spec returns same state"
    (is (= :unknown (fsm/execute :unknown {} {} [:event])))))

(deftest execute-first-matching-transition-wins
  (testing "first matching transition is used"
    (let [spec {:idle {:transitions [[(constantly true) :first]
                                     [(constantly true) :second]]}}]
      (is (= :first (fsm/execute :idle spec {} [:event]))))))

;; ---------------------------------------------------------------------------
;; execute: enter/exit actions
;; ---------------------------------------------------------------------------

(deftest execute-exit-actions-on-leaving
  (testing "exit actions run when leaving a state"
    (let [log (atom [])
          spec {:a {:exit [(fn [_ _] (swap! log conj :exit-a))],
                    :transitions [[(fsm/on :go) :b]]},
                :b {:transitions []}}]
      (fsm/execute :a spec {} [:go])
      (is (= [:exit-a] @log)))))

(deftest execute-enter-actions-on-arriving
  (testing "enter actions run when entering a state"
    (let [log (atom [])
          spec {:a {:transitions [[(fsm/on :go) :b]]},
                :b {:enter [(fn [_ _] (swap! log conj :enter-b))],
                    :transitions []}}]
      (fsm/execute :a spec {} [:go])
      (is (= [:enter-b] @log)))))

(deftest execute-multiple-exit-actions-order
  (testing "multiple exit actions run in order"
    (let [log (atom [])
          spec {:a {:exit [(fn [_ _] (swap! log conj :exit-1))
                           (fn [_ _] (swap! log conj :exit-2))
                           (fn [_ _] (swap! log conj :exit-3))],
                    :transitions [[(constantly true) :b]]},
                :b {:transitions []}}]
      (fsm/execute :a spec {} [:x])
      (is (= [:exit-1 :exit-2 :exit-3] @log)))))

(deftest execute-multiple-enter-actions-order
  (testing "multiple enter actions run in order"
    (let [log (atom [])
          spec {:a {:transitions [[(constantly true) :b]]},
                :b {:enter [(fn [_ _] (swap! log conj :enter-1))
                            (fn [_ _] (swap! log conj :enter-2))],
                    :transitions []}}]
      (fsm/execute :a spec {} [:x])
      (is (= [:enter-1 :enter-2] @log)))))

(deftest execute-exit-before-enter
  (testing "exit runs before enter on transition"
    (let [log (atom [])
          spec {:a {:exit [(fn [_ _] (swap! log conj :exit-a))],
                    :transitions [[(constantly true) :b]]},
                :b {:enter [(fn [_ _] (swap! log conj :enter-b))],
                    :transitions []}}]
      (fsm/execute :a spec {} [:x])
      (is (= [:exit-a :enter-b] @log)))))

;; ---------------------------------------------------------------------------
;; execute: input handling
;; ---------------------------------------------------------------------------

(deftest execute-input-actions-matching
  (testing "input actions execute when condition matches"
    (let [log (atom [])
          spec {:idle {:input [[(constantly true)
                                (fn [_ e] (swap! log conj e))]],
                       :transitions []}}]
      (fsm/execute :idle spec {} [:test])
      (is (= [[:test]] @log)))))

(deftest execute-input-actions-not-matching
  (testing "input actions skip when condition doesn't match"
    (let [log (atom [])
          spec {:idle {:input [[(constantly false)
                                (fn [_ e] (swap! log conj e))]],
                       :transitions []}}]
      (fsm/execute :idle spec {} [:test])
      (is (= [] @log)))))

(deftest execute-multiple-input-conditions
  (testing "all matching input conditions are evaluated"
    (let [log (atom [])
          spec {:idle {:input
                         [[(fsm/on :a) (fn [_ _] (swap! log conj :a-handler))]
                          [(fsm/on :b) (fn [_ _] (swap! log conj :b-handler))]
                          [(constantly true)
                           (fn [_ _] (swap! log conj :catch-all))]],
                       :transitions []}}]
      (fsm/execute :idle spec {} [:a])
      (is (= [:a-handler :catch-all] @log)))))

(deftest execute-input-before-transitions
  (testing "input runs before transitions"
    (let [log (atom [])
          spec {:a {:input [[(constantly true)
                             (fn [_ _] (swap! log conj :input))]],
                    :exit [(fn [_ _] (swap! log conj :exit))],
                    :transitions [[(constantly true) :b]]},
                :b {:enter [(fn [_ _] (swap! log conj :enter))],
                    :transitions []}}]
      (fsm/execute :a spec {} [:x])
      (is (= [:input :exit :enter] @log)))))

;; ---------------------------------------------------------------------------
;; execute: chained transitions
;; ---------------------------------------------------------------------------

(deftest execute-chained-transitions
  (testing "transit chains through multiple states"
    (let [log (atom [])
          spec {:a {:exit [(fn [_ _] (swap! log conj :exit-a))],
                    :transitions [[(constantly true) :b]]},
                :b {:enter [(fn [_ _] (swap! log conj :enter-b))],
                    :exit [(fn [_ _] (swap! log conj :exit-b))],
                    :transitions [[(constantly true) :c]]},
                :c {:enter [(fn [_ _] (swap! log conj :enter-c))],
                    :transitions []}}
          result (fsm/execute :a spec {} [:x])]
      (is (= :c result))
      (is (= [:exit-a :enter-b :exit-b :enter-c] @log)))))

(deftest execute-chain-stops-without-matching
  (testing "chain stops when no transition matches in current state"
    (let [spec {:a {:transitions [[(constantly true) :b]]},
                :b {:transitions [[(constantly false) :c]]},
                :c {:transitions []}}
          result (fsm/execute :a spec {} [:x])]
      (is (= :b result)))))

;; ---------------------------------------------------------------------------
;; execute: context and event passing
;; ---------------------------------------------------------------------------

(deftest execute-context-passed-to-input
  (testing "context is passed to input actions"
    (let [received (atom nil)
          ctx {:user "test"}
          spec {:idle {:input [[(constantly true)
                                (fn [c _] (reset! received c))]],
                       :transitions []}}]
      (fsm/execute :idle spec ctx [:event])
      (is (= ctx @received)))))

(deftest execute-context-passed-to-enter-exit
  (testing "context is passed to enter/exit actions"
    (let [received-exit (atom nil)
          received-enter (atom nil)
          ctx {:data 42}
          spec {:a {:exit [(fn [c _] (reset! received-exit c))],
                    :transitions [[(constantly true) :b]]},
                :b {:enter [(fn [c _] (reset! received-enter c))],
                    :transitions []}}]
      (fsm/execute :a spec ctx [:x])
      (is (= ctx @received-exit))
      (is (= ctx @received-enter)))))

(deftest execute-event-passed-to-all-actions
  (testing "event is passed to input, exit, and enter actions"
    (let [events (atom [])
          event [:click :left]
          spec {:a {:input [[(constantly true)
                             (fn [_ e] (swap! events conj [:input e]))]],
                    :exit [(fn [_ e] (swap! events conj [:exit e]))],
                    :transitions [[(constantly true) :b]]},
                :b {:enter [(fn [_ e] (swap! events conj [:enter e]))],
                    :transitions []}}]
      (fsm/execute :a spec {} event)
      (is (= [[:input event] [:exit event] [:enter event]] @events)))))

(deftest execute-event-passed-to-transition-conditions
  (testing "event is passed to transition condition functions"
    (let [received-event (atom nil)
          spec {:idle {:transitions [[(fn [_ e] (reset! received-event e) true)
                                      :next]]},
                :next {:transitions []}}]
      (fsm/execute :idle spec {} [:my-event :payload])
      (is (= [:my-event :payload] @received-event)))))

;; ---------------------------------------------------------------------------
;; execute!: reactive execution
;; ---------------------------------------------------------------------------

(deftest execute!-updates-atom
  (testing "updates atom with new state"
    (let [state (atom :idle)
          spec {:idle {:transitions [[(fsm/on :start) :running]]},
                :running {:transitions [[(fsm/on :stop) :idle]]}}]
      (fsm/execute! state spec {} [:start])
      (is (= :running @state))
      (fsm/execute! state spec {} [:stop])
      (is (= :idle @state)))))

(deftest execute!-runs-side-effects
  (testing "side effects (enter/exit/input) run during execute!"
    (let [state (atom :idle)
          log (atom [])
          spec {:idle {:exit [(fn [_ _] (swap! log conj :exit-idle))],
                       :transitions [[(fsm/on :go) :active]]},
                :active {:enter [(fn [_ _] (swap! log conj :enter-active))],
                         :transitions []}}]
      (fsm/execute! state spec {} [:go])
      (is (= :active @state))
      (is (= [:exit-idle :enter-active] @log)))))

(deftest execute!-no-transition-keeps-state
  (testing "atom unchanged when no transition matches"
    (let [state (atom :idle)
          spec {:idle {:transitions [[(fsm/on :start) :running]]}}]
      (fsm/execute! state spec {} [:unknown])
      (is (= :idle @state)))))

;; ---------------------------------------------------------------------------
;; Edge cases
;; ---------------------------------------------------------------------------

(deftest empty-enter-exit-actions
  (testing "empty enter/exit action lists cause no errors"
    (let [spec {:a {:enter [], :exit [], :transitions [[(constantly true) :b]]},
                :b {:enter [], :exit [], :transitions []}}
          result (fsm/execute :a spec {} [:x])]
      (is (= :b result)))))

(deftest state-with-no-transitions-key
  (testing "state without :transitions key stays in place"
    (let [spec {:idle {:input [[(constantly true) (fn [_ _] nil)]]}}
          result (fsm/execute :idle spec {} [:x])]
      (is (= :idle result)))))

(deftest self-transition-with-guard
  (testing "self-transition terminates when guard condition changes"
    (let [log (atom [])
          counter (atom 0)
          spec {:a {:exit [(fn [_ _] (swap! log conj :exit))],
                    :enter
                      [(fn [_ _] (swap! counter inc) (swap! log conj :enter))],
                    :transitions [[(fn [_ _] (< @counter 1)) :a]]}}
          result (fsm/execute :a spec {} [:reset])]
      ;; transit recursively re-evaluates transitions after entering new
      ;; state. With always-matching conditions, self-transitions cause
      ;; infinite recursion. Here the guard (< @counter 1) becomes false
      ;; after first enter.
      (is (= :a result))
      (is (= 1 @counter))
      (is (= [:exit :enter] @log)))))
