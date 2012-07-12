(ns objclj.reader
  ; Zetta defines many symbols that conflict with builtins
  (:refer-clojure :exclude [char take-while take])
  (:require [clojure.string :as str])
  (:use clojure.algo.monads)
  (:use [zetta.core :exclude [parse]])
  (:use [zetta.parser.seq :exclude [get ensure whitespace skip-whitespaces]])
  (:use [zetta.combinators :exclude [replicate]]))

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

(defn oneOf [s]
  "Parser that matches any one character in the given string."
  (char (set s)))

(defmacro regex [pat]
  "Parser that matches a regular expression. Returns the matched string."
  `(take-while1 #(re-matches pat %)))

(defmacro always-fn [fn & more]
  "Parser that does not consume any input, and always returns the result of fn."
  `(always (~fn ~@more)))

(defmacro match-escape-seq [seq rep]
  "Parser that matches a backslash followed by seq. Returns rep."
  `(<* (always ~rep)
       (string (str \\ ~seq))))

(defn match-escape-seqs [seqmap]
  "Parser that matches a backslash followed by any key in seqmap. Returns the value associated with the matched key, or the literal escape sequence if no match was found."
  (*> (char \\)
      (<$> #(get seqmap % (str \\ %))
           (oneOf (str (keys seqmap))))))

(def char-in-string
  "Parser that matches a single character or escape sequence. Returns a string."
  (<|> (match-escape-seqs { \t "\t"
                            \b "\b"
                            \n "\n"
                            \r "\r"
                            \f "\f"
                            \' "'"
                            \" "\""
                            \\ "\\" })
       (<$> str (not-char \"))))

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

(def string-literal
  (<$> #(literal-form (str/join %))
       (around (char \") (many char-in-string))))

(defn special-char-literal [ch name]
  "Parser that matches the name of a special character literal. Returns ch."
  (<* (always-fn literal-form ch)
      (string name)))

(def char-literal
  (*> (char \\)
      (choice [(special-char-literal \tab "tab")
               (special-char-literal \space "space")
               (special-char-literal \newline "newline")
               (<$> literal-form any-token)])))

(def form
  (>> skip-whitespaces
      (choice [nil-literal true-literal false-literal number-literal string-literal char-literal
               sym])))

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (-> (parse-once (many form) str) :result))
