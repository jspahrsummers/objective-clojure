(ns objclj.reader
  (:use com.lithinos.amotoen.core))

(def grammar
  {
    :_ (lpegs '| "\n\r\t ,")

    ; allow everything but these characters to begin a symbol
    :symbol-start (lpegs '% "0123456789(){}#\"';@^`~\\")

    ; allow even more characters within a symbol
    :symbol-char (lpegs '% "(){}\"';@`~")

    :symbol [ :symbol-start '(* :symbol-char) ]
    :form [ :symbol ]
  })

(validate grammar)

(defn parse [str]
  "Parses a string of Clojure code into an AST"
  (pegasus :form grammar (gen-ps str)))
