(ns objclj.codegen
  (:use [clojure.core.match :only [match defpred]])
  (:require [clojure.string :as s])
  (:require [objclj.reader :as reader])
  (:use objclj.util))

;;;
;;; Objective-C ASTs and generation
;;;

(defmulti objc
  "Translates an Objective-C element into a string of Objective-C code."
  #(first %))

(defn escape
  "Escapes a single character, to create part of a valid Objective-C identifier. Returns a string."
  [c]
  (str "S" (int c)))

(defn sel-parts
  "Splits a selector into its constituent parts, keeping any colons. Returns a sequence of strings."
  [sel]
  (re-seq #"[a-zA-Z0-9_]+\:?" sel))

(defn method-part
  "Given remaining selector parts and arguments, returns a string representing the rest of an Objective-C message send. selparts and args should both be sequences of strings."
  [selparts args]
  (str
    (cond (empty? selparts) (str ", " (s/join ", " args))
          (empty? args) (str " " (first selparts))
          :else (str " " (first selparts) (first args)))

    ; If we had both a selector part and an argument this time,
    (if (and (and (seq selparts) (seq args))
             ; ... and we have at least one more of either
             (or (next selparts) (next args)))
        ; ... recur
        (method-part (next selparts) (next args)))))

;; Expressions
(derive ::void-expr ::expr)
(derive ::nil-literal ::expr)
(derive ::null-literal ::expr)
(derive ::bool-literal ::expr)
(derive ::number-literal ::expr)
(derive ::character-literal ::expr)
(derive ::nsstring-literal ::expr)
(derive ::selector-literal ::expr)
(derive ::identifier ::expr)
(derive ::message-expr ::expr)
(derive ::nsarray-literal ::expr)

(defmethod objc :void-expr [_]
  "((void)0)")

(defmethod objc :nil-literal [_]
  "((id)nil)")

(defmethod objc :null-literal [_]
  "NULL")

(defmethod objc :bool-literal [[_ b]]
  (if b "@YES" "@NO"))

(defmethod objc :number-literal [[_ n]]
  (str "@" n))

(defmethod objc :character-literal [[_ c]]
  ; TODO: should this just be an NSString? (right now, it'll be an NSNumber)
  (str "@'" c "'"))

(defmethod objc :nsstring-literal [[_ s]]
  (str "@\"" s "\""))

(defmethod objc :selector-literal [[_ s]]
  (str "@selector(" s ")"))

(defmethod objc :identifier [[_ id]]
  (s/replace id #"[^a-zA-Z0-9_]" (comp escape char)))

(defmethod objc :message-expr [[_ obj sel args]]
  (str "["
       (objc obj)
       (method-part (sel-parts sel)
                    (map objc args))
       "]"))

(defmethod objc :nsarray-literal [[_ items]]
  (objc [:message-expr [:identifier "NSArray"]
                       "arrayWithObjects:"
                       (append items [:nil-literal])]))

(defmethod objc :default [_] nil)

;; TODO: statements
;; TODO: basic blocks (function definitions, etc.)

;;;
;;; Translating forms to Objective-C
;;;

;; Predicates for core.match
(defpred number? number?)
(defpred symbol? symbol?)
(defpred keyword? keyword?)
(defpred char? char?)
(defpred string? string?)

(defn gen-form
  "Generates and returns an Objective-C element from a Clojure form. The returned value is suitable for later being passed to the objc function."
  [form]
  (match form
         [:reader/literal true] [:bool-literal true]
         [:reader/literal false] [:bool-literal false]
         [:reader/literal (n :when number?)] [:number-literal n]

         [:reader/literal (c :when char?)] [:character-literal c]

         [:reader/literal (s :when string?)] [:nsstring-literal s]

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

         [:reader/list [[:reader/symbol "."] obj & args]]
           (let [[seltype sel] (first args)
                 args (next args)]
             (if (= :reader/keyword seltype)
                 ; TODO: support non-literal selectors
                 (append [:message-expr (gen-form obj) sel] (map gen-form args))))

         ; TODO
         ;[:reader/list & exprs]

         [:reader/vector items] [:nsarray-literal (map gen-form items)]

         ; TODO: emit NSDictionary literal
         ;[:reader/map keys values]

         _ nil))

;;;
;;; API
;;;

(defn codegen
  "Generates a string of Objective-C code from a sequence of Clojure forms."
  [forms]
  ; TODO: make code generation lazy, so we don't have to keep all the code in memory when outputting to a file
  (s/join "\n" (map #(objc (gen-form %)) forms)))
