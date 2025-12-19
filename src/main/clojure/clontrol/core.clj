(ns clontrol.core
  "Macros to program with delimited continuations."
  (:require
   [clontrol.function
    :as function]
   [clontrol.operator
    :as operator]))

(defmacro ^:private re-export
  {:clj-kondo/lint-as 'clojure.core/def}
  [destination-symbol source-symbol]
  (let [resolved-symbol (resolve source-symbol)]
    `(intern
      *ns*
      (let [export-meta# (meta ~resolved-symbol)]
        (with-meta '~destination-symbol
          (if-let [docstring# (:doc export-meta#)]
            (do
              (assoc
               export-meta#
               :doc
               (str
                docstring#
                "\n\nThis is an alias for [["
                ~(subs (str resolved-symbol) 2)
                "]].")))
            export-meta#)))
      (deref ~resolved-symbol))))

(re-export reset operator/reset)

(re-export shift operator/shift)

(re-export fn-shift function/fn-shift)

(re-export defn-shift function/defn-shift)
