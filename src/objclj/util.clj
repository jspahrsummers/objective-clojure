(ns objclj.util)

(defn append
    "Appends zero or more elements to a collection."
    [coll & elems]
    (concat coll elems))
