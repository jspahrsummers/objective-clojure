(ns objclj.codegen
  (:use [clojure.core.match :only [match defpred]])
  (:require [clojure.string :as s])
  (:require [objclj.reader :as reader]))

;;;
;;; Objective-C ASTs and generation
;;;

(defmulti objc
  "Translates an Objective-C AST into a string of Objective-C code"
  #(first %))

(defn escape [c]
  "Escapes a single character, to create part of a valid Objective-C identifier"
  (str "S" (int c)))

;; Expressions
(derive ::void-expr ::expr)
(derive ::nil-literal ::expr)
(derive ::null-literal ::expr)
(derive ::bool-literal ::expr)
(derive ::number-literal ::expr)
(derive ::selector-literal ::expr)
(derive ::identifier ::expr)
(derive ::message-expr ::expr)

(defmethod objc :void-expr [_]
  "((void)0)")

(defmethod objc :nil-literal [_]
  "((id)nil)")

(defmethod objc :null-literal [_]
  "NULL")

(defmethod objc :bool-literal [[_ b]]
  (if b "YES" "NO"))

(defmethod objc :number-literal [[_ n]]
  (str n))

(defmethod objc :selector-literal [[_ s]]
  (str "@selector(" s ")"))

(defmethod objc :identifier [[_ id]]
  ; TODO: do we need to escape initial digits?
  (s/replace id #"[^a-zA-Z0-9_]" (comp escape char)))

(defmethod objc :message-expr [[_ obj [[_ sel]] & args]]
  (str "["
    (objc obj) " "
    (s/join " " (interleave (s/split #":" sel) args))
    "]"))

(defmethod objc nil [_]
  "")

;;;
;;; Translating forms to Objective-C
;;;

(defpred number? number?)
(defpred symbol? symbol?)
(defpred keyword? keyword?)
(defpred char? char?)
(defpred string? string?)

(defn gen-form [form]
  "Generates an Objective-C AST from a Clojure form"
  (match form
         [:reader/literal true] [:bool-literal true]
         [:reader/literal false] [:bool-literal false]
         [:reader/literal (n :when number?)] [:number-literal n]

         ; TODO
         ;[:reader/literal (c :when char?)]

         ; TODO
         ;[:reader/literal (s :when string?)]

         ; TODO: emit EXTNil
         ;[:reader/literal nil]

         [:reader/symbol sym] [:identifier sym]
         
         ; TODO: intern a selector
         ;[:reader/keyword sym]

         ;; TODO: it's probable that some of these need to be taken care of before hitting the backend

         ; TODO
         ;[:reader/list [[:reader/symbol "def"] [:reader/symbol sym] & init?]]

         ; TODO
         ;[:reader/list [[:reader/symbol "if"] test then & else?]]

         ; TODO
         ;[:reader/list [[:reader/symbol "do"] & exprs]]

         ; TODO
         ;[:reader/list [[:reader/symbol "let"] [:reader/vector bindings] & exprs]]

         ; TODO
         ;[:reader/list [[:reader/symbol "quote"] form]]

         ; TODO
         ;[:reader/list [[:reader/symbol "var"] [:reader/symbol sym]]]

         ; TODO
         ;[:reader/list [[:reader/symbol "fn"] [:reader/vector params] & exprs]]
         ;[:reader/list [[:reader/symbol "fn"] & overloads]]

         ; TODO
         ;[:reader/list [[:reader/symbol "loop"] [:reader/vector bindings] & exprs]]

         ; TODO
         ;[:reader/list [[:reader/symbol "recur"] & exprs]]

         ; TODO
         ;[:reader/list [[:reader/symbol "throw"] expr]]

         ; TODO
         ;[:reader/list [[:reader/symbol "try"] & exprs]]

         ; TODO
         ;[:reader/list [[:reader/symbol "monitor-enter"] x]]

         ; TODO
         ;[:reader/list [[:reader/symbol "monitor-exit"] x]]

         ; TODO
         ;[:reader/list [[:reader/symbol "."] obj sel & args]]
         ;                             ; TODO: support non-literal selectors
         ;                             [:message-expr (gen-form obj) [:selector-literal (str sel)]]
         ;                             (map gen-form args))

         ; TODO
         ;[:reader/list & exprs]


         ; TODO: emit NSArray literal
         ;[:reader/vector items]

         ; TODO: emit NSDictionary literal
         ;[:reader/map keys values]

         _ nil))

;;;
;;; API
;;;

(defn codegen [forms]
  "Generates a string of Objective-C code from a sequence of Clojure forms"
  (doall
    (map #(objc (gen-form %)) forms)))
