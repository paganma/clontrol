(ns clontrol.analyzer.pass.cps-form-emitter
  "Emits a Selective-CPS (Continuation Passing Style) form from an AST node."
  (:require
   [clojure.tools.analyzer.passes
    :refer [schedule]]
   [clojure.tools.analyzer.utils
    :refer [-source-info]]
   [clontrol.analyzer.pass.cps-form-emitter.hole
    :refer [hole->continuation-form
            continuation-form->hole]]
   [clontrol.analyzer.pass.validator
    :refer [validate]]
   [clontrol.analyzer.pass.direct-marker
    :refer [mark-direct]]
   [clontrol.analyzer.pass.shadowings-tagger
    :refer [tag-shadowings]]
   [clontrol.analyzer.pass.form-builder
    :refer [construct-bindings
            prepend-binding
            construct-statements
            prepend-statement]]
   [clontrol.analyzer.pass.function-type-reader
    :refer [read-function-type]]
   [clontrol.analyzer.pass.pure-marker
    :refer [mark-pure]]
   [clontrol.function.shifter
    :refer [invoke-shift
            invoke-unknown]]))


;;;; * Form Emitters

(defmulti emit
  "Emits the CPS form of `node` yielding its result to `plug`."
  (fn [_ _ node]
    (:op node)))

(defn throw-unsupported
  [_
   _
   {operation :op
    form :form
    local-environment :env}]
  (throw
   (ex-info
    (str "Unable to emit nodes of type " operation ".")
    (merge
     {:form form}
     (-source-info form local-environment)))))

(def ^:dynamic *emit-default*
  "The `emit` function called for operations that are not supported by this emitter.

  Default value is [[throw-unsupported]]."
  throw-unsupported)

(defmethod emit
  :default
  [return plug node]
  (*emit-default* return plug node))

(defn emit-original-form
  "Simply returns the `:form` of a `node`."
  [return plug {form :form}]
  (plug return form))

(def ^:dynamic *emit-direct*
  "The `emit` function called for nodes that are marked as `:direct?`.

  Default value is [[emit-original-form]]."
  emit-original-form)

(defn with-node-meta
  "If `form` is able to hold metadata, attaches `source-form`'s meta to `form`."
  [form
   {source-form :form}]
  (if-let [form-meta
           (and (instance? clojure.lang.IObj source-form)
                (meta source-form))]
    (with-meta form
      (merge form-meta (meta source-form)))
    form))

(defn emit-tail
  "Emits the `node`'s form yielding its result to `plug` in tail position."
  [return plug node]
  (if (:direct? node)
    (*emit-direct* return plug node)    
    (emit return plug node)))

(defn emit-cps-form
  "Emits the CPS form of `node` yielding its result to a continuation form. 
  
  Takes the pass option: 

  - `:cps-form-emitter/continuation-form` representing the form of the
  continuation function to which the result of `node`'s form is
  yielded. Defaults to ``identity`."
  {:pass-info
   {:walk :none
    :depends #{#'validate
               #'mark-direct
               #'mark-pure
               #'tag-shadowings}}}
  ([node]
   (trampoline emit-cps-form identity node))
  ([return node]
   (let [passes-options
         (-> node :env :passes-opts)
         continuation-form
         (:cps-form-emitter/continuation-form passes-options `identity)]
     (continuation-form->hole
      (fn [plug]
        (emit-tail return plug node))
      continuation-form))))

(def run-cps-form-emitter
  "Run `emit-cps-form` and related transformations."
  (schedule #{#'emit-cps-form}))

(defn collect
  "Collects `emit`'s result over each `node` in `nodes`, yielding the vector of
  resulting forms to `return`. Each resulting form yield its result to `plug`."
  ([emit return plug nodes]
   (collect emit return plug nodes []))
  ([emit return plug nodes forms]
   (if (seq nodes)
     (let [[node & nodes'] nodes]
       (letfn [(return-next
                 [form]
                 (let [forms' (conj forms form)]
                   #(collect emit return plug nodes' forms')))]
         (emit return-next plug node)))
     (return forms))))

(defn chain
  "Chains `emit`'s result over each `node` in `nodes`, yielding the vector of
  resulting forms to `plug`. For each `node` the result of `emit` is plugged in
  the syntactic context in which the result for the previous `node` was
  emitted. Calls `plug` in a syntactic context in which all of the resulting
  forms have been plugged in sequence."
  ([emit return plug nodes]
   (chain emit return plug nodes []))
  ([emit return plug nodes forms]
   (if (seq nodes)
     (let [[node & nodes'] nodes]
       (letfn [(plug-next
                 [return form]
                 (let [forms' (conj forms form)]
                   #(chain emit return plug nodes' forms')))]
         (emit return plug-next node)))
     (plug return forms))))

(defn- generate-shadow-aliases
  [shadowed-symbols]
  (let [postfix-symbol (gensym "__")]
    (for [shadowed-symbol shadowed-symbols]
      [shadowed-symbol (symbol (str shadowed-symbol postfix-symbol))])))

(defn- save-shadow-aliases
  [form shadow-aliases]
  (if (seq shadow-aliases)
    (let [[[left-symbol right-symbol] & shadow-aliases'] shadow-aliases
          form' (prepend-binding form 'let* right-symbol left-symbol)]
      (recur form' shadow-aliases'))
    form))

(defn- restore-shadow-aliases
  [form shadow-aliases]
  (if (seq shadow-aliases)
    (let [[[left-symbol right-symbol] & shadow-aliases'] shadow-aliases
          form' (prepend-binding form 'let* left-symbol right-symbol)]
      (recur form' shadow-aliases'))
    form))

(defn emit-intermediate
  "Emits the `node`'s form yielding its result to `plug` in an intermediate
  position."
  [return plug node]
  (let [shadowed-symbols (:shadowings node)]
    (if (seq shadowed-symbols)
      (let [aliases (generate-shadow-aliases shadowed-symbols)]
        (emit-tail
         (fn [form]
           (return (save-shadow-aliases form aliases)))
         (fn [return form]
           (let [scope-symbol (gensym "s__")]
             (plug
              (fn [body-form]
                (return
                 (-> body-form
                     (restore-shadow-aliases aliases)
                     (prepend-binding 'let* scope-symbol form))))
              scope-symbol)))
         node))
      (emit-tail return plug node))))

(defn emit-value
  "Emits the `value-node`'s form yielding its result to `plug` in an intermediate
  position in which `value-node`'s value is used. If `value-node` is not
  `:pure?`, the result will be plugged in a syntactic context in which its
  values have been evaluated."
  [return plug value-node]
  (if (:pure? value-node)
    ;; Defer evaluation of the value
    (emit-intermediate return plug value-node)
    ;; Force evaluation of the value before plugging it
    (emit-intermediate
     return
     (fn [return value-form]
       (let [result-symbol (gensym "e__")]
         (plug
          (fn [body-form]
            (return (prepend-binding body-form 'let* result-symbol value-form)))
          result-symbol)))
     value-node)))

(defn emit-values
  "Chains [[emit-value]] over multiple `value-nodes` yielding the vector of
  resulting value forms to `plug`. Plugs the result in a syntactic context in
  which all of the value forms have been bound in sequence, and their effects
  have been evaluated."
  [return plug value-nodes]
  (chain emit-value return plug value-nodes))


;;;; ** Operation Emitters


;;;; *** CASE

(defn emit-case-branch
  [return
   plug
   {hash-code :hash
    test-node :test
    body-node :then}]
  (*emit-direct*
   return
   (fn [return test-form] 
     (emit-tail
      (fn [body-form]
        (return [hash-code [test-form body-form]]))
      plug
      body-node))
   test-node))

(defn emit-case-branches
  [return plug case-then-nodes]
  (collect emit-case-branch return plug case-then-nodes))

(defn emit-case-map
  [return plug test-nodes then-nodes]
  (let [branch-nodes (map merge test-nodes then-nodes)]
    (emit-case-branches
     (fn [case-branch-forms]
       (return (into (sorted-map) case-branch-forms)))
     plug
     branch-nodes)))

(defn emit-case
  [return
   plug
   {test-node :test
    match-nodes :tests
    then-nodes :thens
    default-node :default
    shift :shift
    mask :mask
    switch-type :switch-type
    test-type :test-type
    skip-set :skip-check?
    :as case-node}]
  (emit-value
   return
   (fn [return test-form]
     (emit-case-map
      (fn [case-map]
        (emit-value
         (fn [default-form]
           (plug
            return
            (with-node-meta
              `(case*
                ~test-form
                ~shift ~mask
                ~default-form
                ~case-map
                ~switch-type
                ~test-type
                ~skip-set)
              case-node)))
         plug
         default-node))
      plug
      match-nodes
      then-nodes)
     plug)
   test-node))

(defmethod emit
  :case
  [return plug case-node]
  (emit-case return plug case-node))


;;;; *** DEF

(declare emit-map)

(defn emit-def
  [return
   plug
   {name-symbol :name
    doc-string :doc
    value-node :init
    meta-node :meta
    :as def-node}]
  (emit-map
   return
   (fn [return def-meta-form]
     (let [name-symbol
           (if-let [arglists (:arglists (meta name-symbol))]
             (with-meta name-symbol
               (assoc def-meta-form :arglists (list 'quote arglists)))
             (with-meta name-symbol def-meta-form))
           maybe-doc-string
           (when doc-string [doc-string])]
       (if value-node
         (emit-value
          return
          (fn [return value-form]
            (plug
             return
             (with-node-meta
               `(def ~name-symbol
                  ~@maybe-doc-string
                  ~value-form)
               def-node)))
          value-node)
         (plug
          return
          (with-node-meta
            `(def ~name-symbol)
            def-node)))))
   meta-node))

(defmethod emit
  :def
  [return plug def-node]
  (emit-def return plug def-node))


;;;; *** DO

(defn emit-statement
  "Emits the `statement-node`'s form yielding its result to `plug` in an
  intermediate position in which `statement-node`'s value is *not* used. Plugs
  `nil` in a syntactic context in which the form of `statement-node` has been
  evaluated."
  [return plug statement-node]
  (plug
   (fn [body-form]
     (emit-intermediate
      return
      (fn [return statement-form]
        (return (prepend-statement body-form statement-form)))
      statement-node))
   nil))

(defn emit-statements
  "Chains [[emit-statement]] over multiple `statement-nodes`. Plugs `nil` in a
  syntactic context in which the effects of each statement have been evaluated
  in sequence." 
  [return plug statement-nodes]
  (chain emit-statement return plug statement-nodes))

(defn emit-do
  [return
   plug
   {statement-nodes :statements
    return-node :ret
    :as do-node}]
  (emit-statements
   return
   (fn [return _]
     (emit-tail
      (fn [return-form]
        (return
         (with-node-meta
           `(construct-statements (~return-form))
           do-node)))
      plug
      return-node))
   statement-nodes))

(defmethod emit
  :do
  [return plug do-node]
  (emit-do return plug do-node))


;;;; *** HOST-INTEROP

(defn emit-host-interop
  [return
   plug
   {target-node :target
    host-symbol :m-or-f
    :as host-interop-node}]
  (emit-value
   return
   (fn [return target-form]
     (plug
      return
      (with-node-meta
        `(. ~target-form ~host-symbol)
        host-interop-node)))
   target-node))

(defmethod emit
  :host-interop
  [return plug host-interop-node]
  (emit-host-interop return plug host-interop-node))


;;;; *** IF

(defn emit-if
  [return
   plug
   {test-node :test
    then-node :then
    else-node :else
    :as if-node}]
  (emit-value
   return
   (fn [return test-form]
     (emit-tail
      (fn [then-form]
        (emit-tail
         (fn [else-form]
           (plug
            return
            (with-node-meta
              `(if ~test-form ~then-form ~else-form)
              if-node)))
         plug
         else-node))
      plug
      then-node)
     plug)
   test-node))

(defmethod emit
  :if
  [return plug if-node]
  (emit-if return plug if-node))


;;;; *** INSTANCE-CALL

(defn emit-instance-call
  [return
   plug
   {instance-node :instance
    method-symbol :method
    argument-nodes :args
    :as instance-call-node}]
  (emit-value
   return
   (fn [return target-form]
     (emit-values
      return
      (fn [return argument-forms]
        (plug
         return
         (with-node-meta
           `(. ~target-form ~(cons method-symbol argument-forms))
           instance-call-node)))
      argument-nodes))
   instance-node))

(defmethod emit
  :instance-call
  [return plug instance-call-node]
  (emit-instance-call return plug instance-call-node))


;;;; *** INSTANCE-FIELD

(defn emit-instance-field
  [return
   plug
   {instance-node :instance
    field-symbol :field
    :as instance-field-node}]
  (emit-value
   return
   (fn [return target-form]
     (let [field-symbol (symbol (str ".-" (name field-symbol)))]
       (plug
        return
        (with-node-meta
          `(~field-symbol ~target-form)
          instance-field-node))))
   instance-node))

(defmethod emit
  :instance-field
  [return plug instance-field-node]
  (emit-instance-field return plug instance-field-node))


;;;; *** INVOKE

(defn emit-invoke
  [return
   plug
   {function-node :fn
    argument-nodes :args
    :as invoke-node}]
  (emit-value
   return
   (fn [return function-form]
     (emit-values
      return
      (fn [return argument-forms]
        (case (read-function-type function-node)
          :direct
          (plug
           return
           (with-node-meta
             (list* function-form argument-forms)
             invoke-node))
          :shift
          (hole->continuation-form
           (fn [continuation-form]
             (return
              (with-node-meta
                (list*
                 `invoke-shift
                 function-form
                 continuation-form
                 argument-forms)
                invoke-node)))
           plug)
          #_:default
          (hole->continuation-form
           (fn [continuation-form]
             (return
              (with-node-meta
                (list*
                 `invoke-unknown
                 function-form
                 continuation-form
                 argument-forms)
                invoke-node)))
           plug)))
      argument-nodes))
   function-node))

(defmethod emit
  :invoke
  [return plug invoke-node]
  (emit-invoke return plug invoke-node))


;;;; *** LET

(defn emit-let*-binding
  [return
   plug
   {name-symbol :form
    value-node :init}]
  (plug
   (fn [body-form]
     (emit-intermediate
      return
      ^{:function-form `(fn* ([~name-symbol] ~body-form))}
      (fn [return value-form]
        (return (prepend-binding body-form 'let* name-symbol value-form)))
      value-node))
   name-symbol))

(defn emit-let*-bindings
  [return plug let*-binding-nodes]
  (chain emit-let*-binding return plug let*-binding-nodes))

(defn emit-let*
  [return
   plug
   {binding-nodes :bindings
    body-node :body
    :as let*-node}]
  (emit-let*-bindings
   return
   (fn [return _]
     (emit-tail
      (fn [body-form]
        (return
         (with-node-meta
           `(construct-bindings let* () ~body-form)
           let*-node)))
      plug
      body-node))
   binding-nodes))

(defmethod emit
  :let
  [return plug let*-node]
  (emit-let* return plug let*-node))


;;;; *** LETFN

(defn emit-letfn*-binding
  [return
   plug
   {name-symbol :form
    function-node :init}]
  (*emit-direct*
   return
   (fn [return value-form]
     (plug
      (fn [body-form]
        (return
         (prepend-binding
          body-form
          'letfn*
          name-symbol
          value-form)))
      name-symbol))
   function-node))

(defn emit-letfn*-bindings
  [return plug letfn*-binding-nodes]
  (chain emit-letfn*-binding return plug letfn*-binding-nodes))

(defn emit-letfn*
  [return
   plug
   {binding-nodes :bindings
    body-node :body
    :as letfn*-node}]
  (emit-letfn*-bindings
   (fn [letfn*-form]
     (return (with-node-meta letfn*-form letfn*-node)))
   (fn [return _]
     (emit-tail return plug body-node))
   binding-nodes))

(defmethod emit
  :letfn
  [return plug letfn*-node]
  (emit-letfn* return plug letfn*-node))


;;;; *** LOOP

(defn emit-loop*
  [return
   plug
   {binding-nodes :bindings
    loop-symbol :loop-id
    body-node :body
    :as loop*-node}]
  (emit-let*-bindings
   return
   (fn [return binding-symbols]
     (let [continuation-symbol (gensym "k__")]
       (continuation-form->hole
        (fn [plug']
          (hole->continuation-form
           (fn [continuation-form]
             (emit-tail
              (fn [body-form]
                (return
                 (with-node-meta
                   (list*
                    `(fn* ~loop-symbol
                          ([~continuation-symbol ~@binding-symbols]
                           ~body-form))
                    continuation-form
                    binding-symbols)
                   loop*-node)))
              plug'
              body-node))
           plug))
        continuation-symbol)))
   binding-nodes))

(defmethod emit
  :loop
  [return plug loop*-node]
  (emit-loop* return plug loop*-node))


;;;; *** MAP

(defn emit-map
  [return
   plug
   {value-nodes :vals
    key-nodes :keys}]
  (emit-values
   return
   (fn [return key-value-forms]
     (let [map-entries (map vec (partition 2 key-value-forms))
           map-form (into {} map-entries)]
       (plug return map-form)))
   (interleave key-nodes value-nodes)))

(defmethod emit
  :map
  [return plug set-node]
  (emit-map return plug set-node))


;;;; *** MONITOR-ENTER

(defn emit-monitor-enter
  [return
   plug
   {target-node :target
    :as monitor-enter-node}]
  (emit-value
   return
   (fn [return monitor-target-form]
     (plug
      return
      (with-node-meta
        `(monitor-enter ~monitor-target-form)
        monitor-enter-node)))
   target-node))

(defmethod emit
  :monitor-enter
  [return plug monitor-enter-node]
  (emit-monitor-enter return plug monitor-enter-node))


;;;; *** MONITOR-EXIT

(defn emit-monitor-exit
  [return
   plug
   {target-node :target
    :as monitor-exit-node}]
  (emit-value
   return
   (fn [return monitor-target-form]
     (plug
      return
      (with-node-meta
        `(monitor-exit ~monitor-target-form)
        monitor-exit-node)))
   target-node))

(defmethod emit
  :monitor-exit
  [return plug monitor-exit-node]
  (emit-monitor-exit return plug monitor-exit-node))


;;;; *** NEW

(defn emit-new
  [return
   plug
   {class-node :class
    argument-nodes :args
    :as new-node}]
  (emit-values
   return
   (fn [return argument-forms]
     (*emit-direct*
      return
      (fn [return class-symbol]
        (plug
         return
         (with-node-meta
           (list* `new class-symbol argument-forms)
           new-node)))
      class-node))
   argument-nodes))

(defmethod emit
  :new
  [return plug new-node]
  (emit-new return plug new-node))


;;;; *** RECUR

(defn emit-recur
  [return
   plug
   {argument-nodes :exprs
    loop-symbol :loop-id
    :as recur-node}]
  (emit-values
   return
   (fn [return argument-forms]
     (hole->continuation-form
      (fn [function-form]
        (return
         (with-node-meta
           (list* loop-symbol function-form argument-forms)
           recur-node)))
      plug))
   argument-nodes))

(defmethod emit
  :recur
  [return plug recur-node]
  (emit-recur return plug recur-node))


;;;; *** SET

(defn emit-set
  [return
   plug
   {item-nodes :items
    :as set-node}]
  (emit-values
   return
   (fn [return item-forms]
     (plug
      return
      (with-node-meta
        (set item-forms)
        set-node)))
   item-nodes))

(defmethod emit
  :set
  [return plug set-node]
  (emit-set return plug set-node))


;;;; *** SET!

(defmulti emit-assignee
  (fn [_ _ node]
    (:op node)))

(defn throw-unsupported-assignee
  [_
   _
   {operation :op
    form :form
    local-environment :env}]
  (throw
   (ex-info
    (str "Unable to emit node assignees of type " operation ".")
    (merge
     {:form form}
     (-source-info form local-environment)))))

(def ^:dynamic *emit-assignee-default*
  "The `emit-assignee` function called for assignees that are not supported by
  this emitter. 

  Default value is [[throw-unsupported-assignee]]."
  throw-unsupported-assignee)

(defmethod emit-assignee
  :default
  [return plug node]
  (*emit-assignee-default* return plug node))

(defmethod emit-assignee
  :var
  [return plug node]
  (*emit-direct* return plug node))

(defmethod emit-assignee
  :host-interop
  [return plug node]
  (emit-host-interop return plug node))

(defmethod emit-assignee
  :instance-field
  [return plug node]
  (emit-instance-field return plug node))

(defn emit-set!
  [return
   plug
   {target-node :target
    assigned-node :val
    :as set!-node}]
  (emit-assignee
   return
   (fn [return target-form]
     (emit-value
      return
      (fn [return assigned-form]
        (plug
         return
         (with-node-meta
           `(set! ~target-form ~assigned-form)
           set!-node)))
      assigned-node))
   target-node))

(defmethod emit
  :set!
  [return plug assignment-node]
  (emit-set! return plug assignment-node))


;;;; *** SHIFT

(defn emit-shift*
  [return
   plug
   {handler-node :handler
    argument-nodes :args
    :as shift*-node}]
  (hole->continuation-form
   (fn [continuation-form]
     (emit-value
      return
      (fn [return handler-form]
        (emit-values
         return
         (fn [return argument-forms]
           (return
            (with-node-meta
              (list* handler-form continuation-form argument-forms)
              shift*-node)))
         argument-nodes))
      handler-node))
   plug))

(defmethod emit
  :shift
  [return plug shift-node]
  (emit-shift* return plug shift-node))


;;;; *** STATIC-CALL

(defn emit-static-call
  [return
   plug
   {class-symbol :class
    method-symbol :method
    argument-nodes :args
    :as static-call-node}]
  (emit-values
   return
   (fn [return argument-forms]
     (plug
      return
      (with-node-meta
        `(. ~class-symbol ~(cons method-symbol argument-forms))
        static-call-node)))
   argument-nodes))

(defmethod emit
  :static-call
  [return plug static-call-node]
  (emit-static-call return plug static-call-node))


;;;; *** THROW

(defn emit-throw
  [return
   _ ; Throwing does not yield control to the current continuation.
   {exception-node :exception
    :as throw-node}]
  (emit-value
   return
   (fn [return exception-form]
     (return
      (with-node-meta
        `(throw ~exception-form)
        throw-node)))
   exception-node))

(defmethod emit
  :throw
  [return plug throw-node]
  (emit-throw return plug throw-node))


;;;; *** TRY-CATCH

(defn emit-catch
  ([return
    plug
    {body-node :body
     exception-class-node :class
     binding-node :local
     :as catch-node}]
   (*emit-direct*
    return
    (fn [return exception-class-symbol]
      (*emit-direct*
       return
       (fn [return binding-symbol]
         (emit-tail
          (fn [catch-body-form]
            (return
             (with-node-meta
               `(catch ~exception-class-symbol
                    ~binding-symbol
                  ~catch-body-form)
               catch-node)))
          plug
          body-node))
       binding-node))
    exception-class-node)))

(defn emit-catches
  [return plug catch-nodes]
  (collect emit-catch return plug catch-nodes))

(defn emit-try
  [return
   plug
   {body-node :body
    catch-nodes :catches
    :as try-node}]
  (emit-tail
   (fn [body-form]
     (emit-catches
      (fn [catch-forms]
        (plug
         return
         (with-node-meta
           (list* `try body-form catch-forms)
           try-node)))
      plug
      catch-nodes))
   plug
   body-node)
  plug)

(defn emit-try-finally
  [return
   plug
   {finally-node :finally
    :as try-node}]
  (if finally-node
    ;; General assumption:
    ;; (try ..._1 (finally ..._2))
    ;; Is equivalent to:
    ;; (try (try ..._1 ..._2) (catch Throwable t ..._2 (throw t))
    (emit-try
     (fn [try-form]
       (let [throwable-symbol (gensym "t__")]
         (emit-statement
          (fn [finally-form]
            (return
             (with-node-meta
               `(try
                  ~try-form
                  (catch Throwable ~throwable-symbol
                    ~finally-form))
               finally-node)))
          (fn [return _]
            (return `(throw ~throwable-symbol)))
          finally-node)))
     (fn [return try-form]
       (emit-statement
        return
        (fn [return _]
          (plug return try-form))
        finally-node))
     try-node)
    (emit-try return plug try-node)))

(defmethod emit
  :try
  [return plug try-node]
  (emit-try-finally return plug try-node))


;;;; *** VECTOR

(defn emit-vector
  [return
   plug
   {item-nodes :items
    :as vector-node}]
  (emit-values
   return
   (fn [return item-forms]
     (plug return (with-node-meta item-forms vector-node)))
   item-nodes))

(defmethod emit
  :vector
  [return plug vector-node]
  (emit-vector return plug vector-node))


;;;; *** WITH-META

(defn emit-with-meta*
  [return
   plug
   {argument-node :expr
    meta-node :meta}]
  (emit-map
   return
   (fn [return meta-map]
     (emit
      return
      (fn [return argument-form]
        (plug return (with-meta argument-form meta-map)))
      argument-node))
   meta-node))

(defmethod emit
  :with-meta
  [return plug with-meta*-node]
  (emit-with-meta* return plug with-meta*-node))
