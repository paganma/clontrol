(ns clontrol.analyzer.pass.cps-form-emitter
  "Emits a Selective-CPS (Continuation Passing Style) form from an AST node."
  (:require
   [clojure.tools.analyzer.passes
    :refer [schedule]]
   [clojure.tools.analyzer.utils
    :refer [-source-info]]
   [clontrol.analyzer.pass.control-type-reader
    :refer [read-control-type]]
   [clontrol.analyzer.pass.cps-form-emitter.hole
    :refer [continuation-form->hole
            hole->continuation-form]]
   [clontrol.analyzer.pass.direct-marker
    :refer [mark-direct]]
   [clontrol.analyzer.pass.form-builder
    :refer [construct-bindings
            prepend-binding
            construct-statements
            prepend-statement]]
   [clontrol.analyzer.pass.pure-marker
    :refer [mark-pure]]
   [clontrol.analyzer.pass.recur-dominator-marker
    :refer [mark-recur-dominator]]
   [clontrol.analyzer.pass.shadowings-tagger
    :refer [tag-shadowings]]
   [clontrol.function.shifter
    :refer [call-shift]]))


;;;; * Form Emitters

(defmulti emit
  "Emits the CPS form of `node` yielding its result to `plug`."
  (fn [_ _ node _]
    (:op node)))

(defn throw-unsupported
  [_
   _
   {operation :op
    form :form
    local-environment :env}
   _]
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
  [return plug node context]
  (*emit-default* return plug node context))

(defn emit-original-form
  "Simply returns the `:form` of a `node`."
  [return
   plug
   {form :form}
   context]
  (plug return form context))

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
  [return plug node context]
  (if (:direct? node)
    (*emit-direct* return plug node context)
    (emit return plug node context)))

(defn emit-cps-form
  "Emits the CPS form of `node` yielding its result to a continuation form. 
  
  Takes the pass option: 

  - `:cps-form-emitter/continuation-form` representing the form of the
  continuation function to which the result of `node`'s form is
  yielded. Defaults to ``identity`."
  {:pass-info
   {:walk :none
    :depends #{#'mark-direct
               #'mark-pure
               #'mark-recur-dominator
               #'tag-shadowings}}}
  ([node]
   (let [passes-options
         (-> node :env :passes-opts)
         continuation-form
         (:cps-form-emitter/continuation-form passes-options `identity)]
     (emit-cps-form node continuation-form)))
  ([node continuation-form]
   (trampoline emit-cps-form identity node continuation-form))
  ([return node continuation-form]
   (let [plug (continuation-form->hole continuation-form)]
     (emit-tail return plug node nil))))

(def run-cps-form-emitter
  "Run `emit-cps-form` and related transformations."
  (schedule #{#'emit-cps-form}))

(defn collect
  "Collects `emit`'s result over each `node` in `nodes`, yielding the vector of
  resulting forms to `return`. Each resulting form yield its result to `plug`."
  ([emit return plug nodes context]
   (collect emit return plug nodes context []))
  ([emit return plug nodes context forms]
   (if (seq nodes)
     (let [[node & nodes'] nodes]
       (letfn [(return-next
                 [form]
                 (let [forms' (conj forms form)]
                   #(collect emit return plug nodes' context forms')))]
         (emit return-next plug node context)))
     (return forms))))

(defn chain
  "Chains `emit`'s result over each `node` in `nodes`, yielding the vector of
  resulting forms to `plug`. For each `node` the result of `emit` is plugged in
  the syntactic context in which the result for the previous `node` was
  emitted. Calls `plug` in a syntactic context in which all of the resulting
  forms have been plugged in sequence."
  ([emit return plug nodes context]
   (chain emit return plug nodes context []))
  ([emit return plug nodes context forms]
   (if (seq nodes)
     (let [[node & nodes'] nodes]
       (letfn [(plug-next
                 [return form context]
                 (let [forms' (conj forms form)]
                   #(chain emit return plug nodes' context forms')))]
         (emit return plug-next node context)))
     (plug return forms context))))

(def ^:dynamic *branch-operations*
  #{:if :case :try})

(deftype Result [value])

(defn emit-intermediate
  "Emits the `node`'s form yielding its result to `plug` in an intermediate
  position."
  [return
   plug
   {operation :op
    shadowed-symbols :shadowings
    :as node}
   context]
  (if (or (seq shadowed-symbols)
          (*branch-operations* operation))
    (if (and (:recur-dominator? node) (not (:in-loop? context)))
      (emit-tail
       (fn [result-form]
         (let [result-symbol (gensym "c__")]
           (plug
            (fn [tail-form]
              (return
               (prepend-binding
                `(if (instance? Result ~result-symbol)
                   ~(prepend-binding
                     tail-form
                     'let*
                     result-symbol
                     `(.value ~result-symbol))
                   ~result-symbol)
                'let*
                result-symbol
                result-form)))
            result-symbol
            context)))
       (with-meta
         (fn [return intermediate-form context]
           (if (:in-continuation? context)
             (plug return intermediate-form context)
             (plug
              (fn [_]
                (return `(Result. ~intermediate-form)))
              intermediate-form
              context)))
         (meta plug))
       node
       (assoc context :in-loop? true))
      (hole->continuation-form
       (fn [continuation-form]
         (let [phi-symbol (gensym "p__")]
           (emit-tail
            (fn [tail-form]
              (return
               (prepend-binding
                tail-form
                'let*
                phi-symbol
                continuation-form)))
            ^{:function-form phi-symbol}
            (fn [return intermediate-form _]
              (return `(~phi-symbol ~intermediate-form)))
            node
            context)))
       plug
       context))
    (emit-tail return plug node context)))

(defn emit-value
  "Emits the `value-node`'s form yielding its result to `plug` in an intermediate
  position in which `value-node`'s value is used. If `value-node` is not
  `:pure?`, the result will be plugged in a syntactic context in which its
  values have been evaluated."
  [return plug value-node context]
  (if (:pure? value-node)
    (emit-intermediate return plug value-node context)
    (if (:direct? value-node)
      (*emit-direct*
       return
       (fn [return value-form context]
         (let [result-symbol (gensym "e__")]
           (plug
            (fn [body-form]
              (return (prepend-binding body-form 'let* result-symbol value-form)))
            result-symbol
            context)))
       value-node
       context)
      (emit-intermediate return plug value-node context))))

(defn emit-values
  "Chains [[emit-value]] over multiple `value-nodes` yielding the vector of
  resulting value forms to `plug`. Plugs the result in a syntactic context in
  which all of the value forms have been bound in sequence, and their effects
  have been evaluated."
  [return plug value-nodes context]
  (chain emit-value return plug value-nodes context))


;;;; ** Operation Emitters


;;;; *** CASE

(defn emit-case-branch
  [return
   plug
   {hash-code :hash
    test-node :test
    body-node :then}
   context]
  (*emit-direct*
   return
   (fn [return test-form context] 
     (emit-tail
      (fn [body-form]
        (return [hash-code [test-form body-form]]))
      plug
      body-node
      context))
   test-node
   context))

(defn emit-case-branches
  [return plug case-then-nodes context]
  (collect emit-case-branch return plug case-then-nodes context))

(defn emit-case-map
  [return plug test-nodes then-nodes context]
  (let [branch-nodes (map merge test-nodes then-nodes)]
    (emit-case-branches
     (fn [case-branch-forms]
       (return (into (sorted-map) case-branch-forms)))
     plug
     branch-nodes
     context)))

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
    :as case-node}
   context]
  (emit-value
   return
   (fn [return test-form context]
     (emit-case-map
      (fn [case-map]
        (emit-value
         (fn [default-form]
           (return
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
         default-node
         context))
      plug
      match-nodes
      then-nodes
      context))
   test-node
   context))

(defmethod emit
  :case
  [return plug case-node context]
  (emit-case return plug case-node context))


;;;; *** DEF

(declare emit-map)

(defn emit-def
  [return
   plug
   {name-symbol :name
    doc-string :doc
    value-node :init
    meta-node :meta
    :as def-node}
   context]
  (emit-map
   return
   (fn [return def-meta-form context]
     (let [name-symbol
           (with-meta name-symbol
             (if-let [arglists (:arglists (meta name-symbol))]
               (assoc def-meta-form :arglists (list 'quote arglists))
               def-meta-form))]
       (if value-node
         (emit-value
          return
          (fn [return value-form context]
            (plug
             return
             (with-node-meta
               `(def ~name-symbol
                  ~@(when doc-string [doc-string])
                  ~value-form)
               def-node)
             context))
          value-node
          context)
         (plug
          return
          (with-node-meta
            `(def ~name-symbol)
            def-node)
          context))))
   meta-node
   context))

(defmethod emit
  :def
  [return plug def-node context]
  (emit-def return plug def-node context))


;;;; *** DO

(defn emit-statement
  "Emits the `statement-node`'s form yielding its result to `plug` in an
  intermediate position in which `statement-node`'s value is *not* used. Plugs
  `nil` in a syntactic context in which the form of `statement-node` has been
  evaluated."
  [return plug statement-node context]
  (emit-intermediate
   return
   (fn [return statement-form context]
     (plug
      (fn [body-form]
        (return (prepend-statement body-form statement-form)))
      nil
      context))
   statement-node
   context))

(defn emit-statements
  "Chains [[emit-statement]] over multiple `statement-nodes`. Plugs `nil` in a
  syntactic context in which the effects of each statement have been evaluated
  in sequence." 
  [return plug statement-nodes context]
  (chain emit-statement return plug statement-nodes context))

(defn emit-do
  [return
   plug
   {statement-nodes :statements
    return-node :ret
    :as do-node}
   context]
  (emit-statements
   return
   (fn [return _ context]
     (emit-tail
      (fn [return-form]
        (return
         (with-node-meta
           `(construct-statements (~return-form))
           do-node)))
      plug
      return-node
      context))
   statement-nodes
   context))

(defmethod emit
  :do
  [return plug do-node context]
  (emit-do return plug do-node context))


;;;; *** HOST-INTEROP

(defn emit-host-interop
  [return
   plug
   {target-node :target
    host-symbol :m-or-f
    :as host-interop-node}
   context]
  (emit-value
   return
   (fn [return target-form context]
     (plug
      return
      (with-node-meta
        `(. ~target-form ~host-symbol)
        host-interop-node)
      context))
   target-node
   context))

(defmethod emit
  :host-interop
  [return plug host-interop-node context]
  (emit-host-interop return plug host-interop-node context))


;;;; *** IF

(defn emit-if
  [return
   plug
   {test-node :test
    then-node :then
    else-node :else
    :as if-node}
   context]
  (emit-value
   return
   (fn [return test-form context]
     (emit-tail
      (fn [then-form]
        (emit-tail
         (fn [else-form]
           (return
            (with-node-meta
              `(if ~test-form ~then-form ~else-form)
              if-node)))
         plug
         else-node
         context))
      plug
      then-node
      context))
   test-node
   context))

(defmethod emit
  :if
  [return plug if-node context]
  (emit-if return plug if-node context))


;;;; *** HOST-CALL

(defn emit-host-call
  [return
   plug
   {target-node :target
    method-symbol :method
    argument-nodes :args
    :as host-call-node}
   context]
  (emit-value
   return
   (fn [return target-form context]
     (emit-values
      return
      (fn [return argument-forms context]
        (plug
         return
         (with-node-meta
           (list* '. target-form method-symbol argument-forms)
           host-call-node)
         context))
      argument-nodes
      context))
   target-node
   context))

(defmethod emit
  :host-call
  [return plug host-call-node context]
  (emit-host-call return plug host-call-node context))


;;;; *** HOST-FIELD

(defn emit-host-field
  [return
   plug
   {target-node :target
    field-symbol :field
    :as host-field-node}
   context]
  (emit-value
   return
   (fn [return target-form context]
     (let [field-symbol (symbol (str "." (name field-symbol)))]
       (plug
        return
        (with-node-meta
          `(~field-symbol ~target-form)
          host-field-node)
        context)))
   target-node
   context))

(defmethod emit
  :host-field
  [return plug host-field-node context]
  (emit-host-field return plug host-field-node context))


;;;; *** INVOKE

(defn emit-invoke
  [return
   plug
   {function-node :fn
    argument-nodes :args
    :as invoke-node}
   context]
  (emit-value
   return
   (fn [return function-form context]
     (emit-values
      return
      (fn [return argument-forms context]
        (case (read-control-type function-node)
          :direct
          (plug
           return
           (with-node-meta
             (list* function-form argument-forms)
             invoke-node)
           context)
          :shift
          (hole->continuation-form
           (fn [continuation-form]
             (return
              (with-node-meta
                (list*
                 `call-shift
                 function-form
                 continuation-form
                 argument-forms)
                invoke-node)))
           plug
           context)
          :unknown
          (hole->continuation-form
           (fn [continuation-form]
             (let [result-symbol (gensym "r__")]
               (plug
                (fn [body-form]
                  (return
                   `(if (instance? clontrol.function.shifter.Shifter ~function-form)
                      ~(with-node-meta
                         (list*
                          `call-shift
                          function-form
                          continuation-form
                          argument-forms)
                         invoke-node)
                      ~(prepend-binding
                        body-form
                        'let*
                        result-symbol
                        (with-node-meta
                          (list*
                           function-form
                           argument-forms)
                          invoke-node)))))
                result-symbol
                context)))
           plug
           context)))
      argument-nodes
      context))
   function-node
   context))

(defmethod emit
  :invoke
  [return plug invoke-node context]
  (emit-invoke return plug invoke-node context))


;;;; *** LET

(defn emit-let*-binding
  [return
   plug
   {name-symbol :form
    value-node :init}
   context]
  (emit-intermediate
   return
   (fn [return value-form context]
     (plug
      (fn [body-form]
        (return (prepend-binding body-form 'let* name-symbol value-form)))
      name-symbol
      context))
   value-node
   context))

(defn emit-let*-bindings
  [return plug let*-binding-nodes context]
  (chain emit-let*-binding return plug let*-binding-nodes context))

(defn emit-let*
  [return
   plug
   {binding-nodes :bindings
    body-node :body
    :as let*-node}
   context]
  (emit-let*-bindings
   return
   (fn [return _ context]
     (emit-tail
      (fn [body-form]
        (return
         (with-node-meta
           `(construct-bindings let* () ~body-form)
           let*-node)))
      plug
      body-node
      context))
   binding-nodes
   context))

(defmethod emit
  :let
  [return plug let*-node context]
  (emit-let* return plug let*-node context))


;;;; *** LETFN

(defn emit-letfn*-binding
  [return
   plug
   {name-symbol :form
    function-node :init}
   context]
  (*emit-direct*
   return
   (fn [return value-form context]
     (plug
      (fn [body-form]
        (return
         (prepend-binding
          body-form
          'letfn*
          name-symbol
          value-form)))
      name-symbol
      context))
   function-node
   context))

(defn emit-letfn*-bindings
  [return plug letfn*-binding-nodes context]
  (chain emit-letfn*-binding return plug letfn*-binding-nodes context))

(defn emit-letfn*
  [return
   plug
   {binding-nodes :bindings
    body-node :body
    :as letfn*-node}
   context]
  (emit-letfn*-bindings
   (fn [letfn*-form]
     (return (with-node-meta letfn*-form letfn*-node)))
   (fn [return _ context]
     (emit-tail return plug body-node context))
   binding-nodes
   context))

(defmethod emit
  :letfn
  [return plug letfn*-node context]
  (emit-letfn* return plug letfn*-node context))


;;;; *** LOOP

(defn emit-loop*
  [return
   plug
   {binding-nodes :bindings
    loop-symbol :loop-id
    body-node :body
    :as loop*-node}
   context]
  (emit-let*-bindings
   return
   (fn [return binding-symbols context]
     (let [continuation-symbol (gensym "k__")]
       (emit-tail
        (fn [body-form]
          (hole->continuation-form
           (fn [continuation-form]
             (return
              (with-node-meta
                (list*
                 `(fn* ~loop-symbol
                       ([~continuation-symbol ~@binding-symbols]
                        ~body-form))
                 continuation-form
                 binding-symbols)
                loop*-node)))
           plug
           context))
        (continuation-form->hole continuation-symbol)
        body-node
        (merge context {:in-continuation? false}))))
   binding-nodes
   context))

(defmethod emit
  :loop
  [return plug loop*-node context]
  (emit-loop* return plug loop*-node context))


;;;; *** MAP

(defn emit-map
  [return
   plug
   {value-nodes :vals
    key-nodes :keys}
   context]
  (emit-values
   return
   (fn [return key-value-forms context]
     (let [map-entries (map vec (partition 2 key-value-forms))
           map-form (into {} map-entries)]
       (plug return map-form context)))
   (interleave key-nodes value-nodes)
   context))

(defmethod emit
  :map
  [return plug set-node context]
  (emit-map return plug set-node context))


;;;; *** MONITOR-ENTER

(defn emit-monitor-enter
  [return
   plug
   {target-node :target
    :as monitor-enter-node}
   context]
  (emit-value
   return
   (fn [return monitor-target-form context]
     (plug
      return
      (with-node-meta
        `(monitor-enter ~monitor-target-form)
        monitor-enter-node)
      context))
   target-node
   context))

(defmethod emit
  :monitor-enter
  [return plug monitor-enter-node context]
  (emit-monitor-enter return plug monitor-enter-node context))


;;;; *** MONITOR-EXIT

(defn emit-monitor-exit
  [return
   plug
   {target-node :target
    :as monitor-exit-node}
   context]
  (emit-value
   return
   (fn [return monitor-target-form context]
     (plug
      return
      (with-node-meta
        `(monitor-exit ~monitor-target-form)
        monitor-exit-node)
      context))
   target-node
   context))

(defmethod emit
  :monitor-exit
  [return plug monitor-exit-node context]
  (emit-monitor-exit return plug monitor-exit-node context))


;;;; *** NEW

(defn emit-new
  [return
   plug
   {class-node :class
    argument-nodes :args
    :as new-node}
   context]
  (emit-values
   return
   (fn [return argument-forms context]
     (*emit-direct*
      return
      (fn [return class-symbol context]
        (plug
         return
         (with-node-meta
           (list* `new class-symbol argument-forms)
           new-node)
         context))
      class-node
      context))
   argument-nodes
   context))

(defmethod emit
  :new
  [return plug new-node context]
  (emit-new return plug new-node context))


;;;; *** RECUR

(defn emit-recur
  [return
   plug
   {argument-nodes :exprs
    loop-symbol :loop-id
    :as recur-node}
   context]
  (emit-values
   return
   (fn [return
        argument-forms
        {in-continuation? :in-continuation?}]
     (hole->continuation-form
      (fn [continuation-form]
        (if in-continuation?
          (return
           (with-node-meta
             (list* loop-symbol continuation-form argument-forms)
             recur-node))
          (return
           (with-node-meta
             (list* 'recur continuation-form argument-forms)
             recur-node))))
      plug
      context))
   argument-nodes
   context))

(defmethod emit
  :recur
  [return plug recur-node context]
  (emit-recur return plug recur-node context))


;;;; *** SET

(defn emit-set
  [return
   plug
   {item-nodes :items
    :as set-node}
   context]
  (emit-values
   return
   (fn [return item-forms context]
     (plug
      return
      (with-node-meta
        (set item-forms)
        set-node)
      context))
   item-nodes
   context))

(defmethod emit
  :set
  [return plug set-node context]
  (emit-set return plug set-node context))


;;;; *** SET!

(defmulti emit-assignee
  (fn [_ _ node _]
    (:op node)))

(defn throw-unsupported-assignee
  [_
   _
   {operation :op
    form :form
    local-environment :env}
   _]
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
  [return plug node context]
  (*emit-assignee-default* return plug node context))

(defmethod emit-assignee
  :var
  [return plug node context]
  (*emit-direct* return plug node context))

(defmethod emit-assignee
  :host-interop
  [return plug node context]
  (emit-host-interop return plug node context))

(defmethod emit-assignee
  :host-field
  [return plug node context]
  (emit-host-field return plug node context))

(defn emit-set!
  [return
   plug
   {target-node :target
    assigned-node :val
    :as set!-node}
   context]
  (emit-assignee
   return
   (fn [return target-form context]
     (emit-value
      return
      (fn [return assigned-form context]
        (plug
         return
         (with-node-meta
           `(set! ~target-form ~assigned-form)
           set!-node)
         context))
      assigned-node
      context))
   target-node
   context))

(defmethod emit
  :set!
  [return plug assignment-node context]
  (emit-set! return plug assignment-node context))


;;;; *** SHIFT

(defn emit-shift*
  [return
   plug
   {handler-node :handler
    argument-nodes :args
    :as shift*-node}
   context]
  (hole->continuation-form
   (fn [continuation-form]
     (emit-value
      return
      (fn [return handler-form context]
        (emit-values
         return
         (fn [return argument-forms _]
           (return
            (with-node-meta
              (list* handler-form continuation-form argument-forms)
              shift*-node)))
         argument-nodes
         context))
      handler-node
      context))
   plug
   context))

(defmethod emit
  :shift
  [return plug shift-node context]
  (emit-shift* return plug shift-node context))


;;;; *** THROW

(defn emit-throw
  [return
   _ ; Throwing does not yield control to the current continuation.
   {exception-node :exception
    :as throw-node}
   context]
  (emit-value
   return
   (fn [return exception-form _]
     (return
      (with-node-meta
        `(throw ~exception-form)
        throw-node)))
   exception-node
   context))

(defmethod emit
  :throw
  [return plug throw-node context]
  (emit-throw return plug throw-node context))


;;;; *** TRY-CATCH

(defn emit-catch
  ([return
    plug
    {body-node :body
     exception-class-node :class
     binding-node :local
     :as catch-node}
    context]
   (*emit-direct*
    return
    (fn [return exception-class-symbol context]
      (*emit-direct*
       return
       (fn [return binding-symbol context]
         (emit-tail
          (fn [catch-body-form]
            (return
             (with-node-meta
               `(catch ~exception-class-symbol
                    ~binding-symbol
                  ~catch-body-form)
               catch-node)))
          plug
          body-node
          context))
       binding-node
       context))
    exception-class-node
    context)))

(defn emit-catches
  [return plug catch-nodes context]
  (collect emit-catch return plug catch-nodes context))

(defn emit-try
  [return
   plug
   {body-node :body
    catch-nodes :catches
    :as try-node}
   context]
  (emit-tail
   (fn [body-form]
     (emit-catches
      (fn [catch-forms]
        (return
         (with-node-meta
           (list* 'try body-form catch-forms)
           try-node)))
      plug
      catch-nodes
      context))
   plug
   body-node
   context))

(defn emit-try-finally
  [return
   plug
   {finally-node :finally
    :as try-node}
   context]
  (if finally-node
    (emit-try
     (fn [try-form]
       (let [thrown-symbol (gensym "t__")]
         (emit-statement
          (fn [finally-form]
            (return
             `(try
                ~try-form
                (catch Throwable ~thrown-symbol
                  ~finally-form))))
          (fn [return _ _]
            (return `(throw ~thrown-symbol)))
          finally-node
          context)))
     plug
     try-node
     context)
    (emit-try return plug try-node context)))

(defmethod emit
  :try
  [return plug try-node context]
  (emit-try-finally return plug try-node context))


;;;; *** VECTOR

(defn emit-vector
  [return
   plug
   {item-nodes :items
    :as vector-node}
   context]
  (emit-values
   return
   (fn [return item-forms context]
     (plug return (with-node-meta item-forms vector-node) context))
   item-nodes
   context))

(defmethod emit
  :vector
  [return plug vector-node context]
  (emit-vector return plug vector-node context))


;;;; *** WITH-META

(defn emit-with-meta*
  [return
   plug
   {argument-node :expr
    meta-node :meta}
   context]
  (emit-map
   return
   (fn [return meta-map context]
     (emit
      return
      (fn [return argument-form context]
        (plug return (with-meta argument-form meta-map) context))
      argument-node
      context))
   meta-node
   context))

(defmethod emit
  :with-meta
  [return plug with-meta*-node context]
  (emit-with-meta* return plug with-meta*-node context))
