(ns objclj.monads
  (:use [clojure.algo.monads :rename { domonad m-domonad } :exclude [fetch-val]])
  (:use clojure.test)
  (:use objclj.test))

(defmacro domonad
  "Within a with-monad block, allows Haskell-esque do notation. This macro takes any number of expressions, and rewrites any of the form \"(binding <- monadic-value)\" into the equivalent clojure.algo.monads/domonad structure.

  The last expression given is returned, identically to the final argument of clojure.algo.monads/domonad. <- may not appear in the last expression."
  [& exprs]

  ; TODO: improve this to not require parentheses around (x <- y)
  (let [monad-exprs (map #(if (and (list? %) (= (second %) '<-))
                              (cons (first %) (drop 2 %))
                              (list '_ %))
                    (drop-last exprs))
        flat-exprs (vec (reduce concat monad-exprs))]
    (list `m-domonad flat-exprs (last exprs))))

(deftest test-domonad
  (with-monad (writer-m "")
    (is= [3 "foobar"] (domonad
                        (x <- (m-result 1))
                        (write "foo")
                        (y <- (m-result 2))
                        (write "bar")
                        (+ x y)))))

(with-test
  (defn fetch-val
    "A rewritten version of clojure.algo.monads/fetch-val that does not depend on the key being any particular type."
    [key]
    (with-monad state-m
      (domonad
        (s <- (fetch-state))
        (get s key))))

  (let [st (with-monad state-m
             (domonad (x <- (fetch-val :foo)) (str x)))]
    (is= [":bar" { :foo :bar }] (st { :foo :bar }))))
