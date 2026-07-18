(ns clontrol.analyzer.pass.recur-container-marker
  (:require
   [clojure.tools.analyzer.passes
    :refer [schedule]]
   [clontrol.analyzer.node :as node]))

(defmulti mark-expression :op)

(defmethod mark-expression
  :recur
  [node]
  (assoc node :recur-container? true))

(defmethod mark-expression
  :loop
  [node]
  (assoc node :recur-container? false))

(defmethod mark-expression
  :default
  [node]
  (assoc
   node
   :recur-container?
   (node/some-child? :recur-container? node)))

(def ^:dynamic *mark*
  "The `mark` function used by `mark-recur-container`. 

  Default value is [[mark-expression]]."
  mark-expression)

(defn mark-recur-container
  "Recursively marks a top-level `node` as `:recur-container?`.
  
  A recur-containing node is a node that contains a `recur` operation, and thus
  constitutes an implicit loop."
  {:pass-info
   {:walk :post}}
  ([node]
   (*mark* node)))

(schedule #{#'mark-recur-container})
