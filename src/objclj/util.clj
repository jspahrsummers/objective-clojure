(ns objclj.util
  (:use clojure.test)
  (:use objclj.test))

(with-test
  (defn append
    "Appends zero or more elements to a collection."
    [coll & elems]
    (concat coll elems))

  (is= [:foo] (append [:foo]))
  (is= [:foo :bar] (append [:foo] :bar))
  (is= [:foo :bar :fuzz] (append [:foo] :bar :fuzz))
  (is= [:foo :bar :fuzz] (append [:foo :bar] :fuzz)))

(with-test
  (defn index-of
    "Returns the index of item in sequence s, or nil if it could not be found."
    [item s]
    (let [matches (filter #(= (second %) item) (keep-indexed list s))
          first-match (first matches)]
      (first first-match)))

  (is= 0 (index-of :foo [:foo :bar :fuzz :buzz]))
  (is= 2 (index-of :fuzz [:foo :bar :fuzz :buzz]))
  (is= 2 (index-of :fuzz (list :foo :bar :fuzz :buzz)))
  (is= 1 (index-of \o "foo"))
  (is= nil (index-of :not-here [:foo :bar :fuzz :buzz])))

(defmulti collect-type
  "Given a collection interface and a sequence, returns a concrete collection of that type containing the items in the sequence.
  If the collection type is a map, the sequence should follow the form of [k v k2 v2 k3 v3 ...] and must have an even number of items."
  (fn [t _] t))

(defmethod collect-type clojure.lang.IPersistentVector [_ s]
  (vec s))

(defmethod collect-type clojure.lang.IPersistentList [_ s]
  (if (empty? s) (list) (list* s)))

(defmethod collect-type clojure.lang.IPersistentSet [_ s]
  (set s))

(defmethod collect-type clojure.lang.IPersistentMap [_ s]
  (apply hash-map s))

(defmethod collect-type :default [_ s]
  s)

(with-test #'collect-type
  (is= [1 2 3] (collect-type clojure.lang.IPersistentVector (list 1 2 3)))
  (is= (list 1 2 3) (collect-type clojure.lang.IPersistentList [1 2 3]))
  (is= #{1 2 3} (collect-type clojure.lang.IPersistentSet (list 1 2 3)))
  (is= {:a :b, :foo :bar} (collect-type clojure.lang.IPersistentMap (list :foo :bar, :a :b))))
