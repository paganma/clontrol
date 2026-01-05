(ns clontrol.function
  "Syntax for creating a shift function --- a function which upon invocation
  implicitly captures the continuation up to the nearest enclosing continuation
  prompt."
  (:import
   [clontrol.function.shifter FnShift])
  (:require
   [clontrol.analyzer
    :as analyzer
    :refer [*make-local-environment*
            *scheduled-pass*]]
   [clontrol.analyzer.parser
    :refer [make-local-binding
            parse-local-bindings]]
   [clontrol.analyzer.pass.cps-form-emitter
    :refer [run-cps-form-emitter]]))

;;;; * Function macros
;;; Parsing logic adapted from Clojure's [[fn/defn]] implementation.

;; Copyright (c) Rich Hickey. All rights reserved.  The use and distribution
;; terms for this software are covered by the Eclipse Public License
;; 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in
;; the file epl-v10.html at the root of this distribution.  By using this
;; software in any fashion, you are agreeing to be bound by the terms of this
;; license.  You must not remove this notice, or any other, from this software.


;;;; ** FN-CPS

(defn- maybe-destructured
  "Handles destructured parameters for function arities.

  Adapted from [[clojure.core/maybe-destructured]]."
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur
           (next params)
           (conj new-params (first params))
           lets)
          (let [gparam (gensym "p__")]
            (recur
             (next params)
             (conj new-params gparam)
             (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params (let ~lets ~@body))))))

(defn- parse-fn
  "Parses the signatures of a function form.

  Adapted from [[clojure.core/fn]]."
  [sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                         (if (seq sigs)
                           (str "Parameter declaration "
                                (first sigs)
                                " should be a vector")
                           "Parameter declaration missing")))))
        psig
        (fn [sig]
          ;; Ensure correct type before destructuring sig
          (when (not (seq? sig))
            (throw (IllegalArgumentException.
                    (str "Invalid signature " sig
                         " should be a list"))))
          (let [[params & body] sig
                _ (when (not (vector? params))
                    (throw (IllegalArgumentException.
                            (if (seq? (first sigs))
                              (str "Parameter declaration " params
                                   " should be a vector")
                              (str "Invalid signature " sig
                                   " should be a list")))))
                conds (when (and (next body) (map? (first body)))
                        (first body))
                body (if conds (next body) body)
                conds (or conds (meta params))
                pre (:pre conds)
                post (:post conds)
                body (if post
                       `((let [~'% ~(if (< 1 (count body))
                                      `(do ~@body)
                                      (first body))]
                           ~@(map (fn* [c] `(assert ~c)) post)
                           ~'%))
                       body)
                body (if pre
                       (concat (map (fn* [c] `(assert ~c)) pre)
                               body)
                       body)]
            (maybe-destructured params body)))
        new-sigs (map psig sigs)]
    {:name name
     :signatures new-sigs}))

(defn- emit-fn-cps
  "Generates code for creating a CPS function named `name-symbol` with specified
  `arity-forms`. Uses the local bindings `local-bindings` (provided
  through ([[parse-local-bindings]] &env)."
  ([name-symbol arity-forms local-bindings]
   (emit-fn-cps identity name-symbol arity-forms local-bindings))
  ([return-body name-symbol arity-forms local-bindings]
   (let [continuation-symbol (gensym "k__")]
     `(fn*
       ~name-symbol
       ~@(for [[parameter-symbols & body-forms] arity-forms]
           (let [body-form
                 (list* 'do body-forms)
                 local-bindings
                 (assoc local-bindings name-symbol (make-local-binding name-symbol))
                 local-bindings
                 (into
                  local-bindings
                  (map
                   (fn [parameter-symbol]
                     [parameter-symbol (make-local-binding parameter-symbol)]))
                  parameter-symbols)
                 local-environment
                 (merge
                  (*make-local-environment*)
                  {:passes-opts
                   {:cps-form-emitter/continuation-form continuation-symbol
                    :direct-marker/direct-recur? false}
                   :locals local-bindings
                   :context :ctx/return
                   :loop-id name-symbol
                   :loop-locals (count parameter-symbols)})]
             `([~continuation-symbol ~@parameter-symbols]
               ~(return-body
                 (binding [*scheduled-pass* run-cps-form-emitter]
                   (analyzer/analyze body-form local-environment))))))))))

(defmacro fn-cps
  "Constructor for a CPS function.

  params => positional-params*, or positional-params* & rest-param
  positional-param => binding-form
  rest-param => binding-form
  binding-form => name, or destructuring-form
  
  A `fn-shift` can only be invoked within a continuation prompt (e.g. in a
  `reset` block, or in another `fn-shift`definition), where they can implicitly
  capture the continuation. References to the [[clontrol.operator/shift]]
  operator, as well as other calls to `fn-shift`, are allowed within the
  executable context of the function's body.
  
  Produces an implementor of the [[clontrol.function.shifter/Shifter]]
  protocol. Checkout [[clontrol.function.shifter]] for more information.
  
  Employs the same parsing logic as [[clojure.core/fn]]
  (See https://clojure.org/reference/special_forms#fn for more informaton)."
  [& sigs]
  (let [{name-symbol :name signature-forms :signatures} (parse-fn sigs)
        local-bindings (parse-local-bindings &env)
        name-symbol (or name-symbol (gensym "cps_fn__"))]
    (with-meta
      (emit-fn-cps name-symbol signature-forms local-bindings)
      (meta &form))))


;;;; ** FN-SHIFT

(defn- emit-fn-shift
  "Generates code for creating a SHIFT function named `name-symbol` with specified
  `arity-forms`. Uses the local bindings `local-bindings` (provided
  through ([[parse-local-bindings]] &env)."
  [name-symbol arity-forms local-bindings]
  (let [loop-symbol
        (if name-symbol
          (gensym (str name-symbol "_cps__"))
          (gensym "fn_cps__"))
        local-bindings
        (assoc local-bindings name-symbol (make-local-binding name-symbol))]
    `(FnShift.
      ~(emit-fn-cps
        (fn [body-form]
          (if name-symbol
            `(let* [~name-symbol (FnShift. ~loop-symbol)]
               ~body-form)
            body-form))
        loop-symbol
        arity-forms
        local-bindings))))

(defmacro
  fn-shift
  "Constructor for a SHIFT function.

  params => positional-params*, or positional-params* & rest-param
  positional-param => binding-form
  rest-param => binding-form
  binding-form => name, or destructuring-form
  
  A `fn-shift` can only be invoked within a continuation prompt (e.g. in a
  `reset` block, or in another `fn-shift`definition), where they can implicitly
  capture the continuation. References to the [[clontrol.operator/shift]]
  operator, as well as other calls to `fn-shift`, are allowed within the
  executable context of the function's body.
  
  Produces an implementor of the [[clontrol.function.shifter/Shifter]]
  protocol. Checkout [[clontrol.function.shifter]] for more information.
  
  Employs the same parsing logic as [[clojure.core/fn]]
  (See https://clojure.org/reference/special_forms#fn for more informaton)."
  {:forms '[(fn-shift name? [params*] exprs*)
            (fn-shift name? ([params*] exprs*) +)]
   :clj-kondo/lint-as 'clojure.core/fn}
  [& sigs]
  (let [{name-symbol :name
         signature-forms :signatures}
        (parse-fn sigs)
        local-bindings
        (parse-local-bindings &env)]
    (with-meta
      (emit-fn-shift name-symbol signature-forms local-bindings)
      (meta &form))))


;;;; ** DEFN-CPS

(defn- sigs
  "Normalizes the function declaration `fdecl`.

  Adapted from [[clojure.core/sigs]]."
  [fdecl]
  (let [asig
        (fn [fdecl]
          (let [arglist
                (first fdecl)
                ;; elide implicit macro args
                arglist
                (if (clojure.lang.Util/equals '&form (first arglist))
                  (clojure.lang.RT/subvec
                   arglist
                   2
                   (clojure.lang.RT/count arglist))
                  arglist)
                body
                (next fdecl)]
            (if (map? (first body))
              (if (next body)
                (with-meta arglist
                  (conj
                   (if (meta arglist)
                     (meta arglist)
                     {})
                   (first body)))
                arglist)
              arglist)))
        resolve-tag
        (fn [argvec]
          (let [m (meta argvec)
                ^clojure.lang.Symbol tag (:tag m)]
            (if (instance? clojure.lang.Symbol tag)
              (if (clojure.lang.Util/equiv
                   (.indexOf (.getName tag) ".") -1)
                (if (clojure.lang.Util/equals
                     nil
                     (clojure.lang.Compiler$HostExpr/maybeSpecialTag tag))
                  (let [c (clojure.lang.Compiler$HostExpr/maybeClass tag false)]
                    (if c
                      (with-meta argvec
                        (assoc m :tag (clojure.lang.Symbol/intern (.getName c))))
                      argvec))
                  argvec)
                argvec)
              argvec)))]
    (if (seq? (first fdecl))
      (loop [ret [] fdecls fdecl]
        (if fdecls
          (recur (conj ret (resolve-tag (asig (first fdecls)))) (next fdecls))
          (seq ret)))
      (list (resolve-tag (asig fdecl))))))

(defn- parse-defn
  "Parses the tail of a function definition.

  Adapted from [[clojure.core/defn]]."
  [name & fdecl]
  ;; Note: Cannot delegate this check to def because of the call to
  ;; (with-meta name ..)
  (when (not (instance? clojure.lang.Symbol name))
    (throw
     (IllegalArgumentException.
      "First argument to defn-shift must be a symbol")))

  (let [m (if (string? (first fdecl))
            {:doc (first fdecl)}
            {})
        fdecl (if (string? (first fdecl))
                (next fdecl)
                fdecl)
        m (if (map? (first fdecl))
            (conj m (first fdecl))
            m)
        fdecl (if (map? (first fdecl))
                (next fdecl)
                fdecl)
        fdecl (if (vector? (first fdecl))
                (list fdecl)
                fdecl)
        m (if (map? (last fdecl))
            (conj m (last fdecl))
            m)
        fdecl (if (map? (last fdecl))
                (butlast fdecl)
                fdecl)
        m (conj {:arglists (list 'quote (sigs fdecl))} m)
        m (let [inline (:inline m)
                ifn (first inline)
                iname (second inline)]
            (if (and (= 'fn ifn) (not (symbol? iname)))
              ;; inserts the same fn name to the inline fn if it does not have
              ;; one
              (assoc m :inline (cons
                                ifn
                                (cons
                                 (clojure.lang.Symbol/intern
                                  (.concat (.getName ^clojure.lang.Symbol name)
                                           "__inliner"))
                                 (next inline))))
              m))
        m (conj (if (meta name) (meta name) {}) m)]
    {:name (with-meta name m)
     :function-tail fdecl
     :function-meta {:rettag (:tag m)}}))

(defmacro defn-cps
  "Same as: `(def name (fn-shift [params* ] exprs*))` or 
  `(def name (fn-cps ([params* ] exprs*)+))` with any doc-string or attrs
  added to the var metadata. prepost-map defines a map with optional keys :pre
  and :post that contain collections of pre or post conditions.
  
  See [[clontrol.function/fn-cps]] for more information.

  Employs the same parsing logic and metadata handling
  as [[clojure.core/defn]]."
  {:arglists
   '([name doc-string? attr-map? [params*] prepost-map? body]
     [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])
   :clj-kondo/lint-as 'clojure.core/defn}
  [name & fdecl]
  (let [{name-symbol :name
         function-tail-forms :function-tail
         function-meta :function-meta}
        (parse-defn name fdecl)]
    (list
     'def name-symbol
     (with-meta
       (cons `fn-cps function-tail-forms)
       function-meta))))


;;;; ** DEFN-SHIFT

(defmacro defn-shift
  "Same as: `(def name (fn-shift [params* ] exprs*))` or 
  `(def name (fn-shift ([params* ] exprs*)+))` with any doc-string or attrs
  added to the var metadata. prepost-map defines a map with optional keys :pre
  and :post that contain collections of pre or post conditions.
  
  See [[clontrol.function/fn-shift]] for more information.

  Employs the same parsing logic and metadata handling
  as [[clojure.core/defn]]."
  {:arglists
   '([name doc-string? attr-map? [params*] prepost-map? body]
     [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])
   :clj-kondo/lint-as 'clojure.core/defn}
  [name & fdecl]
  (let [{name-symbol :name
         function-tail-forms :function-tail
         function-meta :function-meta}
        (parse-defn name fdecl)]
    (list
     'def name-symbol
     (with-meta
       (cons `fn-shift function-tail-forms)
       function-meta))))
