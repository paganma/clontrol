(ns clontrol.operator
  "Syntax for delimited control operators."
  (:require
   [clontrol.analyzer
    :as analyzer
    :refer [*make-local-environment*
            *scheduled-pass*]]
   [clontrol.analyzer.parser
    :refer [parse-local-bindings]]
   [clontrol.analyzer.pass.cps-form-emitter
    :refer [run-cps-form-emitter]]))

(defn- emit-reset-to
  "Generates code for a continuation prompt executing `body-form` and yielding its
  result to `continuation-form`. Uses the compiler environment
  `bindings` (provided via `&env` in macros)."
  [continuation-form body-form bindings]
  (let [local-environment
        (merge
         (*make-local-environment*)
         {:passes-opts
          {:cps-form-emitter/continuation-form continuation-form}
          :locals (parse-local-bindings bindings)
          :context :ctx/expr})]
    (binding [*scheduled-pass* run-cps-form-emitter]
      (analyzer/analyze body-form local-environment))))

;;;; * Prompt macros

(defmacro reset-to
  "Establishes a continuation prompt and evaluates `body` within that prompt,
  yielding the result to the `return` function.

  The reduction rules for this operator with respect to
  the [[clontrol.operator/shift]] operator are the following:

  ```clojure
  (reset-to k value)
    -> (k value)
  (reset-to k E[(shift f ps...)])
    -> (reset-to k 
         ((fn-shift [k] (f k ps...))
          (fn [v] (reset-to k (k E[v])))))
  ```"
  [return & body]
  (emit-reset-to
   return
   (with-meta
     (list* 'do body)
     (meta &form))
   &env))

(defmacro reset
  "Danvy and Filinski’s `reset` operator.

  Establishes a prompt and evaluates `body` within that prompt.

  The reduction rules for this operator with respect to
  the [[clontrol.operator/shift]] operator are the following:

  ```clojure
  (reset value)
    -> value
  (reset E[(shift f ps...)])
    -> (reset
         ((fn-shift [k] (f k ps...)) 
          (fn [v] (reset (k E[v])))))
  ```"
  [& body]
  (list* `reset-to `identity body))

;;;; * Continuation capturing macros

(defmacro ^:no-doc shift*
  "Placeholder macro for the [[clontrol.operator/shift]] operator."
  [_ & _]
  (throw (ex-info "Must be called within a continuation prompt." {})))

(defmacro shift
  "Danvy and Filinski’s `shift` operator.

  Aborts to the nearest enclosing prompt and evaluates `function` passing the
  current continuation followed by `arguments`.

  Note that `function` may also be a [[clontrol.function/shift-fn]].

  See [[reset]] and [[reset-to]] for establishing continuation prompts."
  [function & arguments]
  (list* `shift* function arguments))

(defmacro abort
  "Aborts to the nearest enclosing prompt evaluating `value`."
  [value]
  (let [argument-symbol (gensym '_)]
    `(shift (fn* [~argument-symbol] ~value))))
