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
  nil)

; TODO: resolve namespaced symbols
(defn symbol-form
  "Returns a vector representing a Clojure symbol. sym should be a string."
  [sym]
  [ :reader/symbol sym ])

; TODO: resolve double-colon keywords
(defn keyword-form
  "Returns a vector representing a Clojure keyword. kwd should be a string and should not include an initial colon."
  [kwd]
  [ :reader/keyword kwd ])

(defn literal-form
  "Returns a vector representing a Clojure literal value. x may be a string, number, character, boolean, or nil. Use keyword-form for keywords."
  [x]
  [ :reader/literal x ])

(defn vector-form
  "Returns a vector representing a Clojure vector. items may be any kind of sequence."
  [items]
  [ :reader/vector (vec items) ])

(defn list-form
  "Returns a vector representing a Clojure list. items may be any kind of sequence."
  [items]
  [ :reader/list (vec items) ])

(defn map-form
  "Returns a vector representing a Clojure map. pairs should be a sequence of two-item sequences."
  [pairs]
  (let [keys (map #(nth % 0) pairs)
        vals (map #(nth % 1) pairs)]
    [ :reader/map (vec keys) (vec vals) ]))

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

(defmacro always-fn
  "Parser that does not consume any input, and always returns the result of invoking fn with the given arguments."
  [fn & more]
  `(always (~fn ~@more)))

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
  "Parser that matches a symbol. Returns a symbol-form."
  (<$> #(symbol-form (str %1 %2))
       sym-start
       (<$> s/join (many sym-char))))

(def kwd
  "Parser that matches a keyword. Returns a keyword-form."
  (*> (char \:)
      (<$> #(keyword-form (s/join %))
           (many sym-char))))

(def nil-literal
  "Parser that matches literal nil. Returns a literal-form containing nil."
  (<* (always-fn literal-form nil)
      (string "nil")))

(def true-literal
  "Parser that matches literal true. Returns a literal-form containing true."
  (<* (always-fn literal-form true)
      (string "true")))

(def false-literal
  "Parser that matches literal false. Returns a literal-form containing false."
  (<* (always-fn literal-form false)
      (string "false")))

(def number-literal
  "Parser that matches a literal number. Returns a literal-form containing the number."
  ; TODO: BigDecimals
  ; TODO: ratios
  (<$> literal-form number))

(def string-literal
  "Parser that matches a literal string. Returns a literal-form containing the string."
  (<$> #(literal-form (s/join %))
       (around (char \") (many char-in-string))))

(defn special-char-literal
  "Parser that matches reserved character literal name. Returns ch."
  [ch name]
  (<* (always-fn literal-form ch)
      (string name)))

; TODO: this should be a reader macro
(def char-literal
  "Parser that matches a literal character. Returns a literal-form containing the character."
  (*> (char \\)
      (choice [(special-char-literal \tab "tab")
               (special-char-literal \space "space")
               (special-char-literal \newline "newline")
               (<$> literal-form any-token)])))

(declare form)

(defn lst []
  "Parser that matches a list. Returns a list-form."
  (<$> list-form
       (parens (many form))))

(defn vector-literal []
  "Parser that matches a vector. Returns a vector-form."
  (<$> vector-form
       (brackets (many form))))

(defn map-literal []
  "Parser that matches a map. Returns a map-form."
  (<$> map-form
       (braces (many (replicate 2 form)))))

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
  (-> (parse-once (many form) str) :result))
