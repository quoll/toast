(ns ^{:doc "Code for parsing Clojure to an AST for a graph."
      :author "Paula Gearon"}
  toast.core
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :as b]))

(comment
  (def t (slurp "resources/core.cljc")) 
  )

;; Uses a volatile containing a transient vector to accumulate triples representing the data.
;; This is a non-functional approach, but has proven to be significantly faster.
;; The volatile is passed through on the stack, since operating through a dynamic var has
;; significant overhead. However, a dynamic var is used for the current top-level form
;; as this is accessed infrequently.

(def ^:dynamic *current-top-level-form* nil)

(defn new-node
  "A temporary new-node function. Use asami.node/new-node"
  []
  (keyword "t" (name (gensym "n"))))

(defn add-triples
  "Adds triples to the volatile containing the transient triples."
  [vtriples & triples]
  (vswap! vtriples #(reduce conj! % triples)))

(defn wanted-nodes
  "Filters whitespace nodes from a seq of nodes, unless the whitespace prefixes a comment"
  [forms]
  (->> (concat forms [nil])
       (partition 2 1)
       (remove (fn [[f s]] (and (n/whitespace? f) (not (n/comment? s)))))
       (map first)))

(defn to-value
  "Converts tokens into single values.
  If triples are requires to expand on the token value, then these are appended to the vt"
  [vt {:keys [value string-value lines k] :as token}]
  (cond
    value (if (= (str value) (:string-value token))
            value
            (let [n (new-node)]
              (add-triples vt [n :t/value value] [n :t/string-value string-value])
              n))
    lines (first lines)
    k (let [{:keys [auto-resolved? map-qualifier]} token]
        (if (or auto-resolved? map-qualifier)
          (let [n (new-node)]
            (add-triples vt [n :k k])
            (when auto-resolved?
              (add-triples vt [n :t/auto-resolved? auto-resolved?]))
            (when map-qualifier
              (let [{:keys [auto-resolved? prefix]} map-qualifier
                    mq (new-node)]
                (add-triples vt
                             [n :t/map-qualifier mq]
                             [mq :t/auto-resolved? auto-resolved?]
                             [mq :t/prefix prefix])))
            n)
          k))
    :default (throw (ex-info (str "Unknown token type: " token) token))))

(declare create-triples)

(defn list-triples
  "Appends triples to the accumulator to represent a linked list for a given seq.
  Returns a node representing the head of the list"
  [vt s]
  (when (seq s)
    (loop [list-ref nil    ;; reference for the entire list
           last-ref nil    ;; reference for the previously allocated item
           [v & vs :as val-list] s]
      (if-not (seq val-list)
        list-ref
        (let [node-ref (new-node)
              _ (when last-ref (add-triples vt [last-ref :t/rest node-ref]))
              value-ref (create-triples vt v)]
          (add-triples vt [node-ref :t/first value-ref])
          (recur (or list-ref node-ref) node-ref vs))))))

(defn multi-line
  "Converts a multiple part string into a list of strings"
  [vt {:keys [lines] :as multi}]
  (let [list-ref (list-triples vt lines)]
    (add-triples vt [list-ref :t/type :multi-line-string])
    list-ref))

(defn to-seq
  "Converts a seq form into a series of triples which are added to the accumulator, and adds type information"
  [vt form]
  (let [s (wanted-nodes (n/children form))
        list-ref (if (seq s)
                   (list-triples vt s) ;; create the list
                   (new-node))]        ;; no data, so just create an empty node
    (add-triples vt [list-ref :t/type (n/tag form)])
    list-ref))

(defn to-ns-map
  "Similar to a map form in to-seq, but contains namespace information.
  These maps will have the same data type as maps without namespaces, but will have
  :t/auto-resolved? and :t/prefix properties."
  [vt form]
  (let [[{:keys [auto-resolved? prefix]} map-data] (n/children form)
        map-ref (to-seq vt map-data)]
    (add-triples vt [map-ref :t/auto-resolved? auto-resolved?] [map-ref :t/prefix prefix])
    map-ref))

(defn create-triples
  [vtriples form]
  (case (n/tag form)
    :token (to-value vtriples form)
    :multi-line (multi-line vtriples form)
    (:vector :list :set :map) (to-seq vtriples form)
    :namespaced-map (to-ns-map vtriples form)
    (if (string? form)
      form
      (throw (ex-info (str "Unknown type" (n/tag form)) {:form form})))))

(defn form-triples
  [form]
  (let [form-ref (new-node)]
    (if *current-top-level-form*
      (create-triples form)
      (binding [*current-top-level-form* form-ref]
        (create-triples form)))
    form-ref))

(defn top-level-forms
  "Parses the top level forms from text. Removes whitespace between the forms,
  unless it is being used to format comments."
  [text]
  (->> (p/parse-string-all text)
       n/children
       wanted-nodes))

(defn parse-to-triples
  "Converts text to triples."
  [text]
  (let [forms (top-level-forms text)
        triples-acc (volatile! (transient []))
        form-nodes (reduce #(conj %1 (create-triples triples-acc %2)) [] forms)]
    (persistent! @triples-acc)))
