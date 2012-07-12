(ns objclj.codegen
  (:require [clojure.string :as s])
  (:use [clojure.core.match :only [match]])
  (:use clojure.test)
  (:require [objclj.reader :as reader])
  (:use objclj.test)
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

(with-test
  (defn sel-parts
    "Splits a selector into its constituent parts, keeping any colons. Returns a sequence of strings."
    [sel]
    (map second (re-seq #"(:|[a-zA-Z0-9_]+\:?)" sel)))
  
  (is= ["foo"] (sel-parts "foo"))
  (is= ["foo:"] (sel-parts "foo:"))
  (is= ["foo:" "bar:"] (sel-parts "foo:bar:"))
  (is= ["foo:" "bar_:" "with9Things:"] (sel-parts "foo:bar_:with9Things:"))
  (is= ["foo:" ":"] (sel-parts "foo::"))
  (is= [":"] (sel-parts ":"))
  (is= [":" ":"] (sel-parts "::")))

(with-test
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

  (is= " foo" (method-part ["foo"] []))
  (is= " foo:5" (method-part ["foo:"] ["5"]))
  (is= " foo:5 bar:6" (method-part ["foo:" "bar:"] ["5" "6"]))
  (is= " foo:5, 6, 10" (method-part ["foo:"] ["5" "6" "10"])))

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

(defmulti gen-form
  "Generates and returns an Objective-C AST from a Clojure form. The returned value is suitable for later being passed to the objc function."
  #(type %))

(defmethod gen-form nil [_]
  ; TODO: emit EXTNil
  nil)

(defmethod gen-form java.lang.Boolean [b]
  [:bool-literal b])

(defmethod gen-form java.lang.Number [n]
  [:number-literal n])

(defmethod gen-form java.lang.Character [c]
  [:character-literal c])

(defmethod gen-form java.lang.String [s]
  [:nsstring-literal s])

(defmethod gen-form clojure.lang.Symbol [sym]
  [:identifier (str sym)])

(defmethod gen-form clojure.lang.Keyword [kwd]
  ; TODO: intern a selector
  nil)

(defmethod gen-form clojure.lang.IPersistentVector [items]
  [:nsarray-literal (map gen-form items)])

(defmethod gen-form clojure.lang.IPersistentMap [items]
  ; TODO: generate an NSDictionary literal
  nil)

(defmethod gen-form clojure.lang.IPersistentSet [items]
  ; TODO: generate an NSSet literal
  nil)

(defmethod gen-form clojure.lang.IPersistentList [items]
  (match (str (first items))
          "." (let [[_ obj sel & args] items]
                (if (keyword? sel)
                    ; TODO: support non-literal selectors
                    (append [:message-expr (gen-form obj) sel] (map gen-form args))))

          ;; TODO: it's probable that some of these need to be taken care of before hitting the backend

          ; TODO
          "def" (let [[_ sym init] items])

          ; TODO
          "if" (let [[_ test then else] items])

          ; TODO
          "do" (let [[_ & exprs] items])

          ; TODO
          "let" (let [[_ bindings & exprs] items])

          ; TODO
          "loop" (let [[_ bindings & exprs] items])

          ; TODO
          "quote" (let [[_ form] items])

          ; TODO
          "var" (let [[_ sym] items])

          ; TODO
          "fn" (let [[_ & overloads] items])

          ; TODO
          "recur" (let [[_ & exprs] items])

          ; TODO
          "throw" (let [[_ expr] items])

          ; TODO
          "try" (let [[_ & exprs] items])

          ; TODO
          "monitor-enter" (let [[_ x] items])

          ; TODO
          "monitor-exit" (let [[_ x] items])
          
          ; TODO: generate an instance of a CLJList class
          _ nil
  ))

;;;
;;; API
;;;

(defn codegen
  "Generates a string of Objective-C code from a sequence of Clojure forms."
  [forms]
  ; TODO: make code generation lazy, so we don't have to keep all the code in memory when outputting to a file
  (s/join "\n" (map #(objc (gen-form %)) forms)))
