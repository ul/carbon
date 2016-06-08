(ns carbon.rx
  (:refer-clojure :exclude [dosync])
  (:require carbon.rx))

(defmacro rx [& body]
  `(carbon.rx/rx* (fn [] ~@body)))

(defmacro $$ [& body]
  `(carbon.rx/rx* (fn [] ~@body)))

(defmacro lens [getter setter]
  `(carbon.rx/rx* (fn [] ~getter) ~setter))

(defmacro dosync [& body]
  `(carbon.rx/dosync* (fn [] ~@body)))

(defmacro no-rx [& body]
  `(binding [carbon.rx/*rx* nil] ~@body))