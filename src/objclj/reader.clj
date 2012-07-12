(ns objclj.reader
  ; Zetta defines some symbols that conflict with builtins
  (:refer-clojure :exclude [char take-while replicate take])
  (:require [clojure.string :as s])
  (:use clojure.algo.monads)
  (:use clojure.test)
  (:use objclj.test)
  (:use [zetta.core :exclude [parse]])
  (:use [zetta.parser.seq :exclude [get ensure whitespace skip-whitespaces]])
  (:use zetta.combinators))

(defn parse-str
  "Runs parser p on string s and returns a two-item vector containing the result and any unparsed part of the string."
  [p s]
  (let [result (parse-once p s)]
    [(-> result :result) (s/join (-> result :remainder))]))

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

(defmulti strip-empty-forms
  "Collapses all instances of empty-form from the given collection of forms (and all their sub-forms)."
  #(type %))

; TODO: try to eliminate the non-tail recursion in this implementation
(defmacro strip-empty-forms'
  "Like strip-empty-forms, but always returns a sequence."
  [forms]
  `(filter #(not (= empty-form %)) (map strip-empty-forms ~forms)))

(defmethod strip-empty-forms clojure.lang.IPersistentVector [forms]
  (vec (strip-empty-forms' forms)))

(defmethod strip-empty-forms clojure.lang.IPersistentMap [formmap]
  (let [items (interleave (keys formmap) (vals formmap))]
    (apply sorted-map (strip-empty-forms' items))))

(defmethod strip-empty-forms clojure.lang.IPersistentSet [forms]
  (set (strip-empty-forms' forms)))

(defmethod strip-empty-forms clojure.lang.IPersistentList [forms]
  (let [forms' (strip-empty-forms' forms)]
    (if (empty? forms') (list) (list* forms'))))

(defmethod strip-empty-forms :default [form]
  form)

(with-test #'strip-empty-forms
  (is= (list) (strip-empty-forms (list empty-form)))
  (is= (list) (strip-empty-forms (list empty-form empty-form)))
  (is= (list '(true)) (strip-empty-forms (list empty-form (list true empty-form))))
  (is= (list [true]) (strip-empty-forms (list empty-form [true empty-form])))
  (is= (list #{true}) (strip-empty-forms (list empty-form #{ true empty-form })))
  (is= (list {:a :b}) (strip-empty-forms (list empty-form { :a empty-form, empty-form :b })))
  (is= (list {:a :b}) (strip-empty-forms (list empty-form { :a empty-form, :b empty-form })))
  (is= (list {:a :b}) (strip-empty-forms (list empty-form { empty-form :a, :b empty-form }))))

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

(with-test
  (defn match-escape-seq
    "Parser that matches a backslash followed by any character key in seqmap. Returns the value associated with the matched key, or the literal escape sequence if no match was found."
    [seqmap]
    (*> (char \\)
        (<$> #(get seqmap % (str \\ %))
             (oneOf (str (keys seqmap))))))

  (is= ["\n" ""] (parse-str (match-escape-seq {\f "\n", \b "bar"}) "\\f"))
  (is= ["\n" "\\b"] (parse-str (match-escape-seq {\f "\n", \b "bar"}) "\\f\\b"))
  (is= ["bar" ""] (parse-str (match-escape-seq {\f "\n", \b "bar"}) "\\b"))
  (is= [nil "f"] (parse-str (match-escape-seq {\f "\n", \b "bar"}) "f"))
  (is= ["bar" "ar"] (parse-str (match-escape-seq {\f "\n", \b "bar"}) "\\bar")))


(def char-in-string
  "Parser that matches a single character or escape sequence. Returns a string."
  (<|> (match-escape-seq { \t "\t"
                            \b "\b"
                            \n "\n"
                            \r "\r"
                            \f "\f"
                            \' "'"
                            \" "\""
                            \\ "\\" })
       (<$> str (not-char \"))))

(with-test
  ; TODO: this should be a reader macro
  (def line-comment
    "Parser that matches a line comment. Returns empty-form."
    (<* (always empty-form)
        (char \;)
        (many-till any-token
                   (<|> end-of-input eol))))

  (is= [empty-form ""] (parse-str line-comment "; foobar"))
  (is= [empty-form "foobar"] (parse-str line-comment "; foobar\r\nfoobar")))

(with-test
  (def whitespace
    "Parser that matches whitespace and comments. Returns empty-form."
    (<* (always empty-form)
        (<|> (satisfy? whitespace?) line-comment)))

  (is= [empty-form ""] (parse-str whitespace " "))
  (is= [empty-form ""] (parse-str whitespace "\t"))
  (is= [empty-form ""] (parse-str whitespace "\n"))
  (is= [empty-form ""] (parse-str whitespace "\r")))

(with-test
  (def skip-whitespaces
    "Skips whitespace and comments. Returns empty-form."
    (<* (always empty-form)
        (skip-many whitespace)))

  (is= [empty-form ""] (parse-str skip-whitespaces "   \n\t\r ; foo\n    "))
  (is= [empty-form "foo\n"] (parse-str skip-whitespaces "   \n\t\r foo\n")))

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
  (strip-empty-forms (parse-str (many form) str)))
