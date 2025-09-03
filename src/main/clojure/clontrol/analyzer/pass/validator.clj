(ns clontrol.analyzer.pass.validator
  (:import
   [clojure.lang ExceptionInfo])
  (:require
   [clojure.tools.analyzer.passes.source-info
    :refer [source-info]]
   [clojure.tools.analyzer.passes.jvm.validate
    :as jvm-validator]
   [clontrol.analyzer.exception
    :refer [make-compiler-exception]]))

(def validator-pass-info
  (update
   (:pass-info (meta #'jvm-validator/validate))
   :depends
   conj #'source-info))

(defn make-validator-exception
  [file line column cause]
  (let [phase :compile-syntax-check]
    (make-compiler-exception phase file line column nil cause)))

(defn validate
  {:pass-info validator-pass-info}
  [node]
  (try
    (jvm-validator/validate node)
    (catch ExceptionInfo exception
      (let [source-info (ex-data exception)
            {file :file line :line column :column} source-info]
        (if (and file line column)
          (throw (make-validator-exception file line column exception))
          (throw exception))))))
