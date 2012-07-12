(ns objclj.reader
  ; Zetta defines some symbols that conflict with builtins
  (:refer-clojure :exclude [char take-while replicate take])
  (:require [clojure.string :as s])
  (:use clojure.algo.monads)
  (:use [zetta.core :exclude [parse]])
  (:use [zetta.parser.seq :exclude [get ensure whitespace skip-whitespaces]])
  (:use zetta.combinators))

;;;
;;; AST structure
;;;

(def empty-form
  "An empty Clojure form, which should compile down to nothing."
  ::empty-form)

(defn map-form
  "Returns a map from a list of pairs. pairs should be a sequence of two-item sequences."
  [pairs]
  (let [keys (map #(nth % 0) pairs)
        vals (map #(nth % 1) pairs)]
    (zipmap keys vals)))

(defn strip-empty-forms
  "Collapses all instances of empty-form from the given sequence of forms (and all their sub-forms)."
  [forms]

  ; TODO: this could get nasty with too much recursion
  (let [mapped-forms (map #(if (seq? %) (strip-empty-forms %) %) forms)]
    (filter #(not (= empty-form %)) mapped-forms)))

;;;
;;; Character classes
;;;

(defn whitespace? [c]
  "Tests whether a character is considered whitespace in Clojure."
  (or (= c \,) (Character/isWhitespace #^java.lang.Character c)))

;;;
;;; Generic parsers
;;;

(declare skip-whitespaces)

(defn oneOf
  "Parser that matches any one character in the given string. Returns the matched character."
  [s]
  (char (set s)))

(defmacro regex
  "Parser that matches a regular expression. Returns the matched string."
  [pat]
  `(take-while1 #(re-matches pat %)))

(defn surrounded-by
  "Parser that matches character l on the left side of p, and character r on the right side. Returns the result of parser p. Automatically skips spaces within the delimiters."
  [p l r]
  (*> (char l)
      (<* (>> skip-whitespaces p)
          (char r))))

(defn parens
  "Parser that matches parentheses around parser p. Returns the result of parser p."
  [p]
  (surrounded-by p \( \)))

(defn brackets
  "Parser that matches square brackets around parser p. Returns the result of parser p."
  [p]
  (surrounded-by p \[ \]))

(defn braces
  "Parser that matches curly braces around parser p. Returns the result of parser p."
  [p]
  (surrounded-by p \{ \}))

;;;
;;; Clojure-specific parsers
;;;

(defmacro match-escape-seq
  "Parser that matches a backslash followed by seq. Returns rep."
  [seq rep]
  `(<* (always ~rep)
       (string (str \\ ~seq))))

(defn match-escape-seqs
  "Parser that matches a backslash followed by any key in seqmap. Returns the value associated with the matched key, or the literal escape sequence if no match was found."
  [seqmap]
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

; TODO: this should be a reader macro
(def line-comment
  "Parser that matches a line comment. Returns empty-form."
  (<* (always empty-form)
      (char \;)
      (many-till any-token
                 (<|> end-of-input eol))))

(def whitespace
  "Parser that matches whitespace and comments. Returns empty-form."
  (<* (always empty-form)
      (<|> (satisfy? whitespace?) line-comment)))

(def skip-whitespaces
  "Skips whitespace and comments."
  (skip-many whitespace))

(def sym-special-char
  "Parser that matches any non-alphanumeric character that is allowed in a symbol. Returns the matched character."
  (oneOf "*+!-_?/.%&"))

(def sym-start
  "Parser that matches any character that is allowed to begin a symbol. Returns the matched character."
  (<|> letter sym-special-char))

(def sym-char
  "Parser that matches any character that is allowed within (but not necessarily at the beginning of) a symbol. Returns the matched character."
  (choice [sym-start
           digit
           (char \:)]))

(def sym
  "Parser that matches a symbol."
  (<$> #(symbol (str %1 %2))
       sym-start
       (<$> s/join (many sym-char))))

(def kwd
  "Parser that matches a keyword.."
  (*> (char \:)
      (<$> #(keyword (s/join %))
           (many sym-char))))

(def nil-literal
  "Parser that matches literal nil."
  (<* (always nil)
      (string "nil")))

(def true-literal
  "Parser that matches literal true."
  (<* (always true)
      (string "true")))

(def false-literal
  "Parser that matches literal false."
  (<* (always false)
      (string "false")))

(def number-literal
  "Parser that matches a literal number."
  ; TODO: BigDecimals
  ; TODO: ratios
  number)

(def string-literal
  "Parser that matches a literal string."
  (<$> #(s/join %)
       (around (char \") (many char-in-string))))

(defn special-char-literal
  "Parser that matches a reserved character literal name. Returns ch."
  [ch name]
  (<* (always ch)
      (string name)))

; TODO: this should be a reader macro
(def char-literal
  "Parser that matches a literal character."
  (*> (char \\)
      (choice [(special-char-literal \tab "tab")
               (special-char-literal \space "space")
               (special-char-literal \newline "newline")
               (<$> char any-token)])))

(declare form)

(defn lst []
  "Parser that matches a list."
  (<$> list* (parens (many form))))

(defn vector-literal []
  "Parser that matches a vector."
  (<$> vec (brackets (many form))))

(defn map-literal []
  "Parser that matches a map."
  (<$> map-form (braces (many (replicate 2 form)))))

;; TODO: implement reader macros:
;; ' @ ^ #{} #"" #' #() #_ ` ~ ~@

(def form
  "Parser that matches any Clojure form."
  (>> skip-whitespaces
      (choice [nil-literal true-literal false-literal number-literal string-literal char-literal
               (lst) (vector-literal) (map-literal)
               kwd sym])))

(defn parse
  "Parses a string of Clojure code into an AST. Returns a sequence of forms."
  [str]
  (strip-empty-forms (-> (parse-once (many form) str) :result)))
