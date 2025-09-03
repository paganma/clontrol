(ns clontrol.analyzer.parser
  "A parser for producing the AST nodes used by [[clontrol.analyzer]]."
  (:import
   ;; NOTICE: We are making use of the
   ;; `clojure.lang.Compiler$LocalBinding` values in the compiler &env
   ;; map for parsing local bindings, as the keys currently do not
   ;; always preserve the metadata on which we rely (CLJ-2745). This
   ;; is a compiler intrinsic, and therefore once CLJ-2745 is resolved
   ;; we will switch to just using the keys of &env.
   [clojure.lang Compiler$LocalBinding ExceptionInfo])
  (:require
   [clojure.tools.analyzer
    :refer [analyze-form
            analyze-in-env]]
   [clojure.tools.analyzer.jvm
    :as jvm-analyzer]
   [clontrol.analyzer.exception
    :refer [make-compiler-exception]]))

;;;; * Parsing the local environment

(defn- local-binding?
  "Returns `true` if `value` represent a local-binding AST node for a
  local-environment, `false` otherwise."
  [value]
  (and
   (map? value)
   (= (:op value) :binding)
   (contains? value :form)))

(defn make-local-binding
  "Creates a partial AST node for an environment local-binding from a
  `binding-symbol`."
  [binding-symbol]
  {:op :binding
   :name binding-symbol
   :form binding-symbol
   :local :let})

(defn- Compiler$LocalBinding->local-binding
  "Converts a compiler `LocalBinding` into a local-binding AST node."
  [binding]
  (make-local-binding (.sym ^Compiler$LocalBinding binding)))

(defn parse-local-binding
  "Parses a single local-binding AST node from a compiler
  `LocalBinding`. If the input `local-binding` already represents an
  AST node, it returns its argument."
  [local-binding]
  (cond
    (local-binding? local-binding)
    local-binding
    (make-local-binding (.sym ^Compiler$LocalBinding local-binding))
    (Compiler$LocalBinding->local-binding local-binding)
    :else
    (throw
     (ex-info
      (str "Unable to parse local binding: " local-binding)
      {}))))

(defn parse-local-bindings
  "Creates a map of local-bindings from the compiler's `bindings` map,
  which can be referenced within a macro through the implicit `&env`
  parameter."
  [bindings]
  (letfn [(parse-entry
            [[binding-symbol binding]]
            (let [local-binding (parse-local-binding binding)]
              [binding-symbol local-binding]))]
    (into {} (map parse-entry) bindings)))

;;;; * Parsing forms

(def ^:dynamic *parse-default*
  "Default parsing function to be called if the form's headers does not
  match [[clontrol.operator/shift*]].
  
  This function is called with inputs `form`, `local-environment` and
  must output the parsed AST `node`.

  Default value is [[jvm-analyzer/parser]]."
  jvm-analyzer/parse)

(defn parse-shift*
  "Parses a [[clontrol.operator/shift*]] form into a `:shift` AST node."
  [[_ handler-form & argument-forms
    :as form]
   local-environment]
  (let [inner-environment
        (assoc local-environment :context :ctx/expr)
        handler-node
        (analyze-form handler-form inner-environment)
        argument-nodes
        (mapv (analyze-in-env inner-environment) argument-forms)]
    {:op :shift
     :env local-environment
     :form form
     :handler handler-node
     :args argument-nodes
     :children [:handler :args]}))

(defn make-parser-exception
  "Creates a new parsing exception reporting a failure `cause` in
  parsing `symbol` at position `file:line:column`."
  [file line column symbol cause]
  (let [phase :compile-syntax-check]
    (make-compiler-exception phase file line column symbol cause)))

(defn parse
  "Parses a `form` in a `local-environment` into an AST `node`. 

  Parses invocations to the [[clontrol.operator/shift]] operator into
  a `:shift` AST node

  Handles special cases for:

  - [[clontrol.operator/shift*]] operator forms are parsed into
  `:shift` AST nodes.
  - Forms with headers in [[*value-headers*]] are parsed into terminal
  `:value` nodes.
  - Forms with headers in [[*effect-headers*]] are parsed into
  terminal `:effect` nodes.
  - All other forms are parsed by [[*parse-default*]]."
  [form local-environment]
  (let [header (first form)]
    (try
      ((case header
         clontrol.operator/shift* parse-shift*
         #_:else
         *parse-default*)
       form local-environment)
      (catch ExceptionInfo exception
        (let [source-info (ex-data exception)
              {file :file line :line column :column} source-info
              symbol (first form)]
          (throw (make-parser-exception file line column symbol exception)))))))
