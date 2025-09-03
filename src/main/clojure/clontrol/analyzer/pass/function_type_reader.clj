(ns clontrol.analyzer.pass.function-type-reader
  "Utilities to tag the control type of a function."
  (:require
   [clontrol.analyzer.pass.meta-reader
    :refer [read-meta]]))

(def ^:dynamic *direct-function-tag*
  "Meta-tag used to mark direct functions."
  :direct)

(def ^:dynamic *shift-function-tag*
  "Meta-tag used to mark shift functions."
  :shift)

(defn read-function-type
  "Reads the function type of an AST `function-node`, returning either `:direct`
  if it is a direct function, `:shift` if it is a shift function, or `:unknown`
  if the type cannot be determined."
  [function-node]
  (cond
    (read-meta function-node *direct-function-tag*) :direct
    (read-meta function-node *shift-function-tag*) :shift
    (*direct-function-tag* (meta (read-meta function-node :ns))) :direct
    (*shift-function-tag* (meta (read-meta function-node :ns))) :shift
    :else :unknown))
