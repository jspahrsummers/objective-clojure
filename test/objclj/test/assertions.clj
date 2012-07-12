(ns objclj.test.assertions
  (:use clojure.test))

(defmacro is=
  "Asserts that the given arguments compare equal."
  [a b & msg]
  `(is (= ~a ~b) ~@msg))

(defmacro is-not
  "Asserts the opposite of the given predicate."
  [pred & msg]
  `(is (not ~pred) ~@msg))

(defmacro is-not=
  "Asserts that the given arguments do not compare equal."
  [a b & msg]
  `(is-not (= ~a ~b) ~@msg))
