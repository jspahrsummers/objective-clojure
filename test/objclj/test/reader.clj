(ns objclj.test.reader
  (:use clojure.test)
  (:use objclj.reader)
  (:use objclj.test.assertions)
  (:use [zetta.core :only [parse-once]]))

(defn parse-str
  "Helper function to run parser p on string s and return the result."
  [p s]
  (-> (parse-once p s) :result))

(deftest test-line-comment
  (is= empty-form (parse-str whitespace "; foobar"))
  (is-not= empty-form (parse-str whitespace "; foobar\r\nfoobar")))

(deftest test-whitespace
  (is= empty-form (parse-str whitespace " "))
  (is= empty-form (parse-str whitespace "\t"))
  (is= empty-form (parse-str whitespace "\n"))
  (is= empty-form (parse-str whitespace "\r"))
  (is= empty-form (parse-str whitespace "   \n\t\r ; foo\n    ")))
