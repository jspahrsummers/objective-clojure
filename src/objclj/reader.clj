(ns objclj.reader
  (:use com.lithinos.amotoen.core))

;;;
;;; AST cleanup
;;;

(defmulti clean-ast
  "Turns an Amotoen AST into a more representative Clojure data structure"
  #(first (keys %)))

(defmacro make-clean-ast [key val & exprs]
  "Defines a method of multimethod clean-ast, matching against the given key, and destructuring to the given value"
  `(defmethod clean-ast ~key [{ ~val ~key }]
     ~@exprs))

(make-clean-ast :symbol [x xs]
    (symbol (str x (doall xs))))

(make-clean-ast :nil _
    nil)

(make-clean-ast :true _
    true)

(make-clean-ast :false _
    false)

(make-clean-ast :list [_ _ forms _ _]
    nil)

(make-clean-ast :form [_ f]
    (clean-ast f))

;;;
;;; Parsing
;;;

;; The following definitions are not part of the PEG grammar itself, so as to not appear in generated ASTs

; Allow everything but these characters to begin a symbol
(def symbol-start (lpegs '% "0123456789(){}#\"';@^`~\\"))

; Allow even more characters within a symbol
(def symbol-char (lpegs '% "(){}\"';@`~"))

; Single whitespace characters (including commas)
(def whitespace-char (lpegs '| "\n\r\t ,"))

; Semicolon followed by any number of non-newline characters
(def line-comment [ \; '(* (% \newline )) ])

(def grammar
  "PEG grammar for Clojure"
  {
    :_* (list '* whitespace-char line-comment)
    :nil (pegs "nil")
    :true (pegs "true")
    :false (pegs "false")
    :symbol [ symbol-start (list '* symbol-char) ]
    :list [ \( '(* :form) :_* \) ]
    :form [ :_* '(| :nil :true :false :list :symbol ) ]
  })

(validate grammar)

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (let [ast (pegasus :form grammar (gen-ps str))]
    ; TODO: For debugging only
    (println ast)
    (clean-ast ast)))
