(ns clontrol.analyzer
  "Analyzer applying AST transformations."
  (:require
   [clojure.tools.analyzer
    :as analyzer]
   [clojure.tools.analyzer.env
    :refer [ensure]
    :rename {ensure ensure-global-environment}]
   [clojure.tools.analyzer.jvm
    :as jvm-analyzer]
   [clontrol.analyzer.parser
    :as parser]
   [clontrol.analyzer.preprocessor
    :as preprocessor]))


;;;; * Analysis extension points

(def ^:dynamic *scheduled-pass*
  "Transformation pass ran by [[analyze]] after constructing the AST.
  
  Default value is [[jvm-analyzer/scheduled-default-passes]]."
  jvm-analyzer/scheduled-default-passes)

(def ^:dynamic *create-var*
  "Creates a var from a symbol.

  Default value is [[jvm-analyzer/create-var]]."
  jvm-analyzer/create-var)

(def ^:dynamic *parse*
  "Parses a form into an AST node.

  Default value is [[parser/parse]]."
  parser/parse)

(def ^:dynamic *var?*
  "Returns `true` if the argument is a var, `false` otherwise.

  Default value is [[clojure.core/var?]]."
  var?)

(def ^:dynamic *macroexpand-1*
  "Macro-expands the argument form once. Used by [[analyze]] to expand macros
  before parsing the AST.

  Default value is [[preprocessor/macroexpand-1]]."
  preprocessor/macroexpand-1)

(defn global-environment?
  "Returns `true` if `value` is a valid global environment, `false` otherwise."
  [value]
  (and
   (instance? clojure.lang.IAtom value)
   (map? @value)
   (contains? @value :namespaces)
   (contains? @value :update-ns-map!)))

(def ^:dynamic *make-global-environment*
  "Thunk instantiating an atom containing the global analysis environment.

  Default value is [[jvm-analyzer/global-env]]."
  jvm-analyzer/global-env)

(defn local-environment?
  "Returns `true` if `value` is a valid local environment, `false` otherwise."
  [value]
  (and
   (map? value)
   (contains? value :locals)
   (map? (:locals value))
   (contains? value :context)
   (contains? value :ns)))

(def ^:dynamic *make-local-environment*
  "Thunk instantiating an empty local analysis environment.

  Default value is [[jvm-analyzer/empty-env]]."
  jvm-analyzer/empty-env)


;;;; * Analysis entry point

(defn analyze
  "Analyzes an input `form` using [[clojure.tools.analyzer/analyze]] binding:
  
  - [[*create-var*]] to [[analyzer/create-var]]
  - [[*parse*]] to [[analyzer/parse]]
  - [[*var?*]] to [[analyzer/var?]]
  - [[*macroexpand-1*]] to [[analyzer/macroexpand-1]]

  The `form` will be parsed within the local environment specified by the
  parameter `local-environment`, and global environment specified by the binding
  [[clojure.tools.analyzer.env.*env*]]. If the local environment is not passed,
  it defaults to the result of [[*make-local-environment*]]; if the global
  environment is unbound, it is bound to the result
  of [[*make-global-environment*]].

  Runs [[*scheduled-pass*]] on the parsed AST."
  ([form]
   (analyze form (*make-local-environment*)))
  ([form local-environment]
   (binding
       [analyzer/create-var *create-var*
        analyzer/parse *parse*
        analyzer/var? *var?*
        analyzer/macroexpand-1 *macroexpand-1*]
       (ensure-global-environment
        (*make-global-environment*)
        (*scheduled-pass*
         (analyzer/analyze form local-environment))))))
