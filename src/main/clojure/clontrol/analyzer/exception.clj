(ns clontrol.analyzer.exception
  "Utilities for creating exceptions embedding source information."
  (:import
   ;; NOTICE: We are making use of the internal
   ;; `clojure.lang.Compiler$CompilerException` implementation, as simply using
   ;; ex-info with `clojure.error` attributes won't be able to properly
   ;; communicate the error's location to Cider, if the error is thrown during
   ;; macroexpansion time.
   [clojure.lang Compiler$CompilerException]))

(defn compiler-exception?
  "Returns `true` if `exception` is an instance of
  `clojure.lang.Compiler$CompilerException`, `false` otherwise."
  [exception]
  (instance? Compiler$CompilerException exception))

(defn make-compiler-exception
  "Creates a new compiler exception in a compiler where:

  - `phase` must be one of the following keywords: `:read-source`,
  `:compile-syntax-check`, `:compilation`, `:macro-syntax-check`
  `:macroexpansion`, `:execution`, `:read-eval-result`, `:print-eval-result`.
  - `file`, `line`, `column` must represent the location related to the
     exception.
  - `symbol` (optional) must be the symbol that could not be parsed or
  macroexpanded.
  - `cause` must be the underlying exception that caused this compiler
  exception."
  ([phase file line column cause]
   (make-compiler-exception phase file line column nil cause))
  ([phase file line column symbol cause]
   (Compiler$CompilerException. file line column symbol phase cause)))
