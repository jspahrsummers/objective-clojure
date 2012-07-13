(ns objclj.monads
  (:use [clojure.algo.monads :exclude [fetch-val]])
  (:use clojure.test)
  (:use objclj.test))

(with-test
  (defn fetch-val
    "A rewritten version of clojure.algo.monads/fetch-val that does not depend on the key being any particular type."
    [key]
    (domonad state-m [s (fetch-state)] (get s key)))

  (let [st (domonad state-m [x (fetch-val :foo)]
             (str x))]
    (is= [":bar" { :foo :bar }] (st { :foo :bar }))))

(defmacro run-state-t
  "Given an initial state, evaluates a state computation, and returns a two-item sequence of the result and the final state, wrapped in the underlying monad."
  [st initial]
  `(~st ~initial))

(defmacro eval-state-t
  "Given an initial state, evaluates a state computation, and returns the result wrapped in the underlying monad."
  [st initial]
  `((m-lift 1 first) (run-state-t ~st ~initial)))
