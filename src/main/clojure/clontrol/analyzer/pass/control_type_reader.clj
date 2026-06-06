(ns clontrol.analyzer.pass.control-type-reader
  "Utilities to tag the control type of a function."
  (:require
   [clontrol.analyzer.pass.meta-reader
    :refer [read-meta]]))

(defn- find-namespaces
  [pattern]
  (into
   #{}
   (filter
    (fn [namespace]
      (re-find pattern (str namespace))))
   (all-ns)))

(def ^:private ^:const direct-namespace-pattern
  (re-pattern "clojure\\..*"))

(def ^:dynamic *direct-namespaces*
  (find-namespaces direct-namespace-pattern))

(def ^:dynamic *shift-namespaces*
  #{})

(def ^:dynamic *direct-tag*
  "Meta-tag used to mark direct functions."
  :direct)

(def ^:dynamic *shift-tag*
  "Meta-tag used to mark shift functions."
  :shift)

(defn read-control-type-from-meta
  "Reads the function type of an AST `function-node`, returning either `:direct`
  if it contains the `:direct` tag, `:shift` if it contains a `:shift` tag, or
  `:unknown` if the type cannot be determined."
  [function-node]
  (let [function-meta (read-meta function-node)
        namespace (:ns function-meta)
        namespace-meta (meta namespace)]
    (cond
      (*direct-tag* function-meta) :direct
      (*shift-tag* function-meta) :shift
      (*direct-tag* namespace-meta) :direct
      (*shift-tag* namespace-meta) :shift
      (*direct-namespaces* namespace) :direct
      (*shift-namespaces* namespace) :shift
      :else :unknown)))

(def ^:dynamic *read-control-type*
  "The function used by [[direct-marker]] to extract the control type of a
  `function-node`. Possible values returned by this function are:

  - `direct`: If the function can be called directly i.e. without capturing the
    continuation.
  - `shift`: If the function must be called in CPS.
  - `unknown`: If the function may be called either directly or in CPS.

  Default value is [[read-control-type-from-meta]]"
  read-control-type-from-meta)
