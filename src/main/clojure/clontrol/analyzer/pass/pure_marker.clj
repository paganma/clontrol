(ns clontrol.analyzer.pass.pure-marker
  "Marks the pure nodes in an AST."
  (:require
   [clojure.tools.analyzer.passes
    :refer [schedule]]
   [clontrol.analyzer.node
    :as node]))

(def ^:dynamic *pure-operations*
  "Operations that can be considered pure regardless of the respective
  subtree."
  #{:const
    :deftype
    :local
    :fn
    :maybe-class
    :maybe-host-form
    :reify
    :static-field
    :value})

(def ^:dynamic *compound-pure-operations*
  "Operations that can be considered pure only if their children are
  pure as well."
  #{:binding
    :catch
    :const
    :do
    :if
    :instance-field
    :keyword-invoke
    :let
    :letfn
    :local
    :loop
    :map
    :quote
    :recur
    :set
    :shift
    :the-var
    :try
    :var
    :vector
    :with-meta})

(declare ^:dynamic *mark*)

(defn- mark-children
  [return node]
  (node/update-children return *mark* node))

(defn mark-expression
  "Marks an expression as `:pure?`. An expression can be marked pure if and only
  if all of its children are also direct."
  [return
   {operation :op
    :as node}]
  (if (*pure-operations* operation)
    (return (assoc node :pure? true))
    #(mark-children
      (fn [{operation :op
            :as node}]
        (return
         (cond
           (*compound-pure-operations* operation)
           (assoc node :pure? (node/every-child? :pure? node))
           :else
           (dissoc node :pure?))))
      node)))

(def ^:dynamic *mark*
  "The `mark` function used by `mark-pure`.

  Default value is [[mark-expression]]."
  mark-expression)

(defn mark-pure
  "Recursively marks a top-level `node` as `:pure?`.
  
  A pure node is a node that does not have side-effects."
  {:pass-info
   {:walk :none}}
  ([node]
   (trampoline mark-pure identity node))
  ([return node]
   (*mark* return node)))

(def run-pure-marker
  (schedule #{#'mark-pure}))
