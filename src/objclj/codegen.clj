(ns objclj.codegen
  (:use [clojure.core.match :only [match]]))

;;;
;;; Objective-C ASTs and generation
;;;

(def bool-literal)

(defn objc [ast]
  "Translates an Objective-C AST into a string of Objective-C code"
  (match ast
         [bool-literal true] "YES"
         [bool-literal false] "NO"
         _ nil))

;;;
;;; Translating forms to Objective-C
;;;

(defn gen-form [form]
  "Generates an Objective-C AST from a Clojure form"
  (match form
         true [bool-literal true]
         false [bool-literal false]

         _ nil))

;;;
;;; API
;;;

(defn codegen [& forms]
  "Generates a string of Objective-C code from Clojure forms"
  (doall
    (map #(objc (gen-form %)) forms)))
