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
  (let [function-meta (read-meta function-node)
        namespace-meta (meta (:ns function-meta))]
    (cond
      (*direct-function-tag* function-meta) :direct
      (*shift-function-tag* function-meta) :shift
      (*direct-function-tag* namespace-meta) :direct
      (*shift-function-tag* namespace-meta) :shift
      :else :unknown)))
