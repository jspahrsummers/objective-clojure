(ns objclj.codegen
  (:use objclj.preds)
  (:use [clojure.core.match :only [match]]))

;;;
;;; Objective-C ASTs and generation
;;;

(defmulti objc
  "Translates an Objective-C AST into a string of Objective-C code"
  #(first %))

;; Expressions
(derive ::void-expr ::expr)
(derive ::nil-literal ::expr)
(derive ::null-literal ::expr)
(derive ::bool-literal ::expr)
(derive ::number-literal ::expr)
(derive ::selector-literal ::expr)

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

;;;
;;; Translating forms to Objective-C
;;;

(defn gen-form [form]
  "Generates an Objective-C AST from a Clojure form"
  (match form
         true [:bool-literal true]
         false [:bool-literal false]

         (n :when number?) [:number-literal n]

         _ nil))

;;;
;;; API
;;;

(defn codegen [& forms]
  "Generates a string of Objective-C code from Clojure forms"
  (doall
    (map #(objc (gen-form %)) forms)))
