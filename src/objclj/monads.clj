(ns objclj.monads
  (:use [clojure.algo.monads :rename { domonad m-domonad }])
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

(deftest monads
  (with-monad (writer-m "")
    (is= [3 "foobar"] (domonad
                        (x <- (m-result 1))
                        (write "foo")
                        (y <- (m-result 2))
                        (write "bar")
                        (+ x y)))))
