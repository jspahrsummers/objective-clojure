(ns objclj.util
  (:use clojure.test)
  (:use objclj.test))

(with-test
  (defn append
    "Appends zero or more elements to a collection."
    [coll & elems]
    (concat coll elems))

  (is= [:foo] (append [:foo]))
  (is= [:foo :bar] (append [:foo] :bar))
  (is= [:foo :bar :fuzz] (append [:foo] :bar :fuzz))
  (is= [:foo :bar :fuzz] (append [:foo :bar] :fuzz)))
