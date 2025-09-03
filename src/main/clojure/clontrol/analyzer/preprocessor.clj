(ns clontrol.analyzer.preprocessor
  "A preprocessor for macro-expanding forms before they are parsed
  by [[clontrol.analyzer]]."
  (:refer-clojure
   :exclude [macroexpand-1])
  (:require
   [clojure.tools.analyzer.jvm
    :as jvm-analyzer]
   [clojure.tools.analyzer.utils
    :refer [-source-info]]
   [clontrol.analyzer.exception
    :refer [make-compiler-exception]]))

(def ^:dynamic *macroexpand-1-default*
  "Default macro-expansion function to be called if the form's headers
  does not match [[clontrol.operator/shift*]].

  Default value is [[jvm-analyzer/macroexpand-1]]."
  jvm-analyzer/macroexpand-1)

(defn make-macro-exception
  "Creates a new macro-expansion exception reporting a failure `cause`
  in macro-expanding `symbol` at position `file:line:column`."
  [file line column symbol cause]
  (let [phase :macroexpansion]
    (make-compiler-exception phase file line column symbol cause)))

(defn macroexpand-1
  "Calls [[*macroexpand-1-default*]] if the form does not constitute a
  `shift*` operation."
  [form local-environment]
  (if (and (seq? form) (= (first form) 'clontrol.operator/shift*))
    form
    (try
      (*macroexpand-1-default* form local-environment)
      (catch Exception exception
        (let [source-info (-source-info form local-environment)
              {file :file line :line column :column} source-info
              symbol (first form)]
          (throw (make-macro-exception file line column symbol exception)))))))
