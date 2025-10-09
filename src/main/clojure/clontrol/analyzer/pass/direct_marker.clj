(ns clontrol.analyzer.pass.direct-marker
  "Marks the direct nodes in an AST."
  (:require
   [clojure.tools.analyzer.passes
    :refer [schedule]]
   [clontrol.analyzer.pass.control-type-reader
    :refer [read-control-type]]
   [clontrol.analyzer.node
    :as node]))

(def ^:dynamic *direct-operations*
  "Operations that can be considered direct regardless of the respective
  subtree."
  #{:case-test
    :const
    :deftype
    :fn
    :import
    :keyword
    :local
    :quote
    :reify
    :static-field
    :the-var
    :var})

(def ^:dynamic *compound-direct-operations*
  "Operations that are considered direct only if all of the elements in their
  subtree are direct as well."
  #{:binding
    :case
    :case-then
    :catch
    :def
    :default
    :do
    :host-call
    :host-field
    :host-interop
    :if
    :instance-call
    :instance-field
    :let
    :letfn
    :map
    :monitor-enter
    :monitor-exit
    :new
    :recur
    :set
    :set!
    :static-call
    :throw
    :try
    :vector
    :with-meta})

(defmulti mark
  (fn [_ node]
    (:op node)))

(declare unmark-recur-path)

(defn mark-direct
  "Recursively marks a top-level `node` as `:direct?`. 

  A direct node is a node that does not contain a reference to the `:shift`
  operator, or invocations to indirect functions for all its executions.

  If `node` is in the tail context, `recur` operations will also be treated as
  indirect operations."
  {:pass-info
   {:walk :none}}
  ([node]
   (trampoline mark-direct identity node))
  ([return node]
   (mark
    (fn [node]
      (if (node/tail-node? node)
        (unmark-recur-path return node)
        (return node)))
    node)))

(def run-direct-marker
  (schedule #{#'mark-direct}))

(defn- mark-children
  [return node]
  (node/update-children return mark node))

(defn mark-expression
  "Marks an expression as `:direct?` using the following strategy:

  - An expression node in [[*direct-operations*]] is always marked as direct.
  - An expression node in [[*compound-direct-operations*]] can be marked as direct
  if and only if all of its children are also direct."
  [return
   {operation :op
    :as node}]
  (if (*direct-operations* operation)
    (return (assoc node :direct? true))
    #(mark-children
      (fn [node]
        (return
         (if (*compound-direct-operations* operation)
           (assoc node :direct? (node/every-child? :direct? node))
           (dissoc node :direct?))))
      node)))

(def ^:dynamic *mark-default*
  "The `mark` function used to mark operations that are not invocations or loops.

  Default value is [[mark-expression]]."
  mark-expression)

(defmethod mark
  :default
  [return node]
  (*mark-default* return node))

(defn mark-invoke
  [return invoke-node]
  (mark-children
   (fn [{function-node :fn
         :as invoke-node}]
     (return
      (if (and (= (read-control-type function-node) :direct)
               (node/every-child? :direct? invoke-node))
        (assoc invoke-node :direct? true)
        invoke-node)))
   invoke-node))

(defmethod mark
  :invoke
  [return invoke-node]
  (mark-invoke return invoke-node))

(defn unmark-recur-path
  [return node]
  (node/update-tails
   (fn [{operation :op
         :as node}]
     (return
      (if (and
           (:direct? node)
           (or (= operation :recur)
               (not (node/every-tail? :direct? node))))
        (dissoc node :direct?)
        node)))
   (fn [return
        {operation :op
         :as child-node}]
     (if (= operation :loop)
       (return child-node)
       #(unmark-recur-path return child-node)))
   node))

(defn mark-loop
  [return loop-node]
  (mark-children
   (fn [loop-node]
     (if (node/every-child? :direct? loop-node)
       (return (assoc loop-node :direct? true))
       (unmark-recur-path return loop-node)))
   loop-node))

(defmethod mark
  :loop
  [return loop-node]
  (mark-loop return loop-node))
