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
    :refer [capture-hole
            continuation-form->hole
            hole->continuation-form]]
   [clontrol.analyzer.pass.direct-marker
    :refer [mark-direct]]
   [clontrol.analyzer.pass.form-builder
    :refer [construct-bindings
            construct-statements
            prepend-binding
            prepend-statement]]
   [clontrol.analyzer.pass.pure-marker
    :refer [mark-pure]]
   [clontrol.analyzer.pass.shadowings-tagger
    :refer [tag-shadowings]]
   [clontrol.operator.intrinsic.thunk
    :refer [thunk]
    :as thunk]
   [clontrol.function.shifter
    :refer [call-shift
            call-unknown
            shifter?]]))


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
  [return
   plug
   {form :form}]
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
    :depends #{#'mark-direct
               #'mark-pure
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
   (let [plug (continuation-form->hole continuation-form)
         {{context :context
           passes-options :passes-opts} :env} node]
     (emit-tail
      (fn [form]
        (return
         (if (and (:cps-form-emitter/thunk-recur? passes-options)
                  (not (= context :ctx/return)))
           `(thunk/trampoline ~form)
           form)))
      plug
      node))))

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

(defn emit-value
  "Emits the `value-node`'s form yielding its result to `plug` in an intermediate
  position in which `value-node`'s value is used. If `value-node` is not
  `:pure?`, the result will be plugged in a syntactic context in which its
  values have been evaluated."
  [return plug value-node]
  (if (:pure? value-node)
    (emit-tail return plug value-node)
    (if (:direct? value-node)
      (*emit-direct*
       return
       (fn [return value-form]
         (let [effect-symbol (gensym "e__")]
           (plug
            (fn [body-form]
              (return
               (prepend-binding
                body-form
                'let*
                effect-symbol
                value-form)))
            effect-symbol)))
       value-node)
      (emit-tail return plug value-node))))

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
  (capture-hole
   (fn [return plug]
     (emit-value
      return
      (fn [return test-form]
        (emit-case-map
         (fn [case-map]
           (emit-tail
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
            default-node))
         plug
         match-nodes
         then-nodes))
      test-node))
   return
   plug))

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
           (with-meta name-symbol
             (if-let [arglists (:arglists (meta name-symbol))]
               (assoc def-meta-form :arglists (list 'quote arglists))
               def-meta-form))]
       (if value-node
         (emit-value
          return
          (fn [return value-form]
            (plug
             return
             (with-node-meta
               `(def ~name-symbol
                  ~@(when doc-string [doc-string])
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
  (emit-tail
   return
   (fn [return statement-form]
     (plug
      (fn [body-form]
        (return (prepend-statement body-form statement-form)))
      nil))
   statement-node))

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
  (capture-hole
   (fn [return plug]
     (emit-value
      return
      (fn [return test-form]
        (emit-tail
         (fn [then-form]
           (emit-tail
            (fn [else-form]
              (return
               (with-node-meta
                 `(if ~test-form ~then-form ~else-form)
                 if-node)))
            plug
            else-node))
         plug
         then-node))
      test-node))
   return
   plug))

(defmethod emit
  :if
  [return plug if-node]
  (emit-if return plug if-node))


;;;; *** HOST-CALL

(defn emit-host-call
  [return
   plug
   {target-node :target
    method-symbol :method
    argument-nodes :args
    :as host-call-node}]
  (emit-value
   return
   (fn [return target-form]
     (emit-values
      return
      (fn [return argument-forms]
        (plug
         return
         (with-node-meta
           (list* '. target-form method-symbol argument-forms)
           host-call-node)))
      argument-nodes))
   target-node))

(defmethod emit
  :host-call
  [return plug host-call-node]
  (emit-host-call return plug host-call-node))


;;;; *** HOST-FIELD

(defn emit-host-field
  [return
   plug
   {target-node :target
    field-symbol :field
    :as host-field-node}]
  (emit-value
   return
   (fn [return target-form]
     (let [field-symbol (symbol (str "." (name field-symbol)))]
       (plug
        return
        (with-node-meta
          `(~field-symbol ~target-form)
          host-field-node))))
   target-node))

(defmethod emit
  :host-field
  [return plug host-field-node]
  (emit-host-field return plug host-field-node))


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
        (case (read-control-type function-node)
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
                 `call-shift
                 function-form
                 continuation-form
                 argument-forms)
                invoke-node)))
           plug)
          :unknown
          (hole->continuation-form
           (fn [continuation-form]
             (return
              (with-node-meta
                (list*
                 `call-unknown
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
  (emit-tail
   return
   (fn [return value-form]
     (plug
      (fn [body-form]
        (return
         (prepend-binding
          body-form
          'let*
          name-symbol
          value-form)))
      name-symbol))
   value-node))

(defn emit-let*-bindings
  [return plug let*-binding-nodes]
  (chain emit-let*-binding return plug let*-binding-nodes))

(defn capture-closure-hole
  [return
   return-body
   plug
   {shadowed-symbols :shadowings}]
  (if (seq shadowed-symbols)
    (capture-hole return return-body plug)
    (return return-body plug)))

(defn emit-let*
  [return
   plug
   {binding-nodes :bindings
    body-node :body
    :as let*-node}]
  (capture-closure-hole
   (fn [return plug]
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
   return
   plug
   let*-node))

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
           plug))
        (continuation-form->hole continuation-symbol)
        body-node)))
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
    {passes-options :passes-opts} :env
    :as recur-node}]
  (emit-values
   return
   (fn [return argument-forms]
     (hole->continuation-form
      (fn [continuation-form]
        (return
         (let [recur-form (list* loop-symbol continuation-form argument-forms)]
           (with-node-meta
             (if (:cps-form-emitter/thunk-recur? passes-options)
               `(thunk ~recur-form)
               recur-form)
             recur-node))))
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
  :host-field
  [return plug node]
  (emit-host-field return plug node))

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
    {passes-options :passes-opts} :env
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
   (if (:cps-form-emitter/thunk-recur? passes-options)
     (fn [return form]
       (plug
        (fn [form]
          (return `(thunk/trampoline ~form)))
        form))
     plug)))

(defmethod emit
  :shift
  [return plug shift-node]
  (emit-shift* return plug shift-node ))


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
        (return
         (with-node-meta
           (list* 'try body-form catch-forms)
           try-node)))
      plug
      catch-nodes))
   plug
   body-node))

(defn emit-try-finally
  [return
   plug
   {finally-node :finally
    :as try-node}]
  (capture-hole
   (fn [return plug]
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
             (fn [return _]
               (return `(throw ~thrown-symbol)))
             finally-node)))
        plug
        try-node)
       (emit-try return plug try-node)))
   return
   plug))

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
