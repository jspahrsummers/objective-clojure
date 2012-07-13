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
