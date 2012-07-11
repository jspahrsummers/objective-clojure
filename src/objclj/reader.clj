(ns objclj.reader
  ; Zetta defines many symbols that conflict with builtins
  (:refer-clojure :exclude [ensure, get, char, take-while, take, replicate])
  (:use clojure.algo.monads)
  (:use [zetta.core :exclude [parse]])
  (:use [zetta.parser.seq :exclude [whitespace, skip-whitespaces]])
  (:use zetta.combinators))

;;;
;;; Parsers
;;;

(defn is-whitespace [c]
  (or (= c \,) (Character/isWhitespace #^java.lang.Character c)))

(def whitespace
  (satisfy? is-whitespace))

(def skip-whitespaces
  (skip-many whitespace))

(def nil-literal
  (<* (always nil)
      (string "nil")))

(def true-literal
  (<* (always true)
      (string "true")))

(def false-literal
  (<* (always false)
      (string "false")))

(def parser
  (>> skip-whitespaces
      (choice [nil-literal true-literal false-literal])))

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (parse-once parser str))
