;;; Reusable predicates for core.match
(ns objclj.preds
  (:use [clojure.core.match :only [defpred]]))

(defpred number? number?)
