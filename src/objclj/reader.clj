(ns objclj.reader
  (:use com.lithinos.amotoen.core))

;; The following definitions are not part of the PEG grammar itself, so as to not appear in generated ASTs

; Allow everything but these characters to begin a symbol
(def symbol-start (lpegs '% "0123456789(){}#\"';@^`~\\"))

; Allow even more characters within a symbol
(def symbol-char (lpegs '% "(){}\"';@`~"))

; Single whitespace characters (including commas)
(def whitespace-char (lpegs '| "\n\r\t ,"))

; Semicolon followed by any number of non-newline characters
(def line-comment [ \; '(* '(% \newline )) ])

(def grammar
  {
    :_* (list '* whitespace-char line-comment)
    :nil (pegs "nil")
    :true (pegs "true")
    :false (pegs "false")
    :symbol [ symbol-start (list '* symbol-char) ]
    :form [ :_* '(| :nil :true :false :symbol ) ]
  })

(validate grammar)

(defmulti clean-ast
  "Turns an Amotoen AST into a more representative Clojure data structure"
  #(first (keys %)))

(defmethod clean-ast :symbol [{ [x xs] :symbol }]
    (symbol (str x (doall xs))))

(defmethod clean-ast :nil [{ _ :nil }]
    nil)

(defmethod clean-ast :true [{ _ :true }]
    true)

(defmethod clean-ast :false [{ _ :false }]
    false)

(defmethod clean-ast :form [{ [_ f] :form }]
    (clean-ast f))

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (let [ast (pegasus :form grammar (gen-ps str))]
    ; TODO: For debugging only
    (println ast)
    (clean-ast ast)))
