(ns objclj.reader
  ; Zetta defines many symbols that conflict with builtins
  (:refer-clojure :exclude [ensure, get, char, take-while, take, replicate])
  (:require [clojure.string :as str])
  (:use clojure.algo.monads)
  (:use [zetta.core :exclude [parse]])
  (:use [zetta.parser.seq :exclude [whitespace, skip-whitespaces]])
  (:use zetta.combinators))

;;;
;;; AST structure
;;;

(def empty-form
  "An empty Clojure form, which should compile down to nothing."
  nil)

(defn symbol-form [str]
  "Returns a vector representing a symbol."
  [ :reader/symbol str ])

(defn literal-form [x]
  "Returns a vector representing a literal value."
  [ :reader/literal x ])

;;;
;;; Character classes
;;;

(defn whitespace? [c]
  (or (= c \,) (Character/isWhitespace #^java.lang.Character c)))

;;;
;;; Parsers
;;;

(defn oneOf [str]
  "Parser that matches any one character in the given string."
  (char (set str)))

(defmacro regex [pat]
  "Parser that matches a regular expression. Returns the matched string."
  `(take-while1 #(re-matches pat %)))

(defmacro always-fn [fn & more]
  "Parser that does not consume any input, and always returns the result of fn."
  `(always (~fn ~@more)))

(def line-comment
  "Parser that matches a line comment. Returns nil."
  (<* (always empty-form)
      (char \;)
      (many-till any-token
                 (<|> end-of-input eol))))

(def whitespace
  (<|> (satisfy? whitespace?) line-comment))

(def skip-whitespaces
  (skip-many whitespace))

(def sym-special-char
  (oneOf "*+!-_?/.%:&"))

(def sym-start
  (<|> letter sym-special-char))

(def sym-char
  (<|> sym-start digit))

(def sym
  (<$> #(symbol-form (str %1 %2))
       sym-start
       (<$> str/join (many sym-char))))

(def nil-literal
  (<* (always-fn literal-form nil)
      (string "nil")))

(def true-literal
  (<* (always-fn literal-form true)
      (string "true")))

(def false-literal
  (<* (always-fn literal-form false)
      (string "false")))

(def number-literal
  ; TODO: BigDecimals
  ; TODO: ratios
  (<$> literal-form number))

(def form
  (>> skip-whitespaces
      (choice [nil-literal true-literal false-literal number-literal
               sym])))

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (-> (parse-once (many form) str) :result))
