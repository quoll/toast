(ns ^{:doc "Code for parsing Clojure to an AST for a graph"
      :author "Paula Gearon"}
  toast.core
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]))

  ;; a comment

#?(:clj (def N ^{:vector true} [5])
   :cljs (def N ^{:vector true} [4]))

(defn wanted-nodes
  [forms]
  (->> (partition 2 1)
       (remove (fn [[f s]] (and (n/whitespace? f) (not (n/comment? s)))))
       (map first)))

(defn create-triples
  [triples form]
  )

(defn top-level-forms
  [text]
  (->> (p/parse-string-all text)
       n/children
       wanted-nodes))

(defn parse-to-triples
  [text]
  (let [forms (top-level-forms text)]
    (reduce create-triples [] forms)))
