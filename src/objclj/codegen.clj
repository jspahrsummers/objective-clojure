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

(defmethod objc nil [_] "")

;;;
;;; Translating forms to Objective-C
;;;

(defpred number? number?)
(defpred symbol? symbol?)
(defpred keyword? keyword?)

(defn gen-form [form]
  "Generates an Objective-C AST from a Clojure form"
  (match form
         [:reader/literal true] [:bool-literal true]
         [:reader/literal false] [:bool-literal false]
         [:reader/literal (n :when number?)] [:number-literal n]

         [:reader/symbol sym] [:identifier sym]

         ;(['. obj sel & args] :seq) (concat
         ;                             ; TODO: support non-literal selectors
         ;                             [:message-expr (gen-form obj) [:selector-literal (str sel)]]
         ;                             (map gen-form args))

         _ nil))

;;;
;;; API
;;;

(defn codegen [forms]
  "Generates a string of Objective-C code from a sequence of Clojure forms"
  (doall
    (map #(objc (gen-form %)) forms)))
