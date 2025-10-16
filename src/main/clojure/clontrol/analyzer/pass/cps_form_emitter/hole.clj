(ns clontrol.analyzer.pass.cps-form-emitter.hole
  (:require
   [clontrol.analyzer.pass.form-builder
    :refer [prepend-binding]]))

(def plug-into-identity
  ^{:function-form `identity}
  (fn [return form] (return form)))

(defn continuation-form->hole
  [continuation-form]
  (if (= continuation-form `identity)
    plug-into-identity
    ^{:function-form continuation-form}
    (fn [return form]
      (return `(~continuation-form ~form)))))

(def ^:dynamic *in-continuation?* false)

(defn hole->continuation-form
  [plug]
  (if-let [function-form (:function-form (meta plug))]
    function-form
    (let [argument-symbol
          (gensym "x__")
          body-form
          (binding [*in-continuation?* true]
            (trampoline plug identity argument-symbol))]
      `(fn* ([~argument-symbol] ~body-form)))))

(defn reify-hole
  "Reifies `plug` into a continuation form `phi` and returns two holes:

  - `plug-outer` declaring the function `phi`.
  - `plug-inner` yielding the result o the computation to `phi`.

  This is used to avoid the generation of redundant code for branching
  operations, for which multiple branches may yield to the same
  continuation (see [[emit-if]], [[emit-case]], [[emit-try]])."
  [return plug]
  (let [continuation-form (hole->continuation-form plug)]
    (if (symbol? continuation-form)
      (return plug-into-identity plug)
      (let [phi-symbol
            (gensym "p__")
            bind-outer
            (fn [return outer-form]
              (return
               (prepend-binding
                outer-form
                'let*
                phi-symbol
                continuation-form)))
            bind-inner
            ^{:function-form phi-symbol}
            (fn [return inner-form]
              (return `(~phi-symbol ~inner-form)))]
        (return bind-outer bind-inner)))))

(deftype ClosureResult
    [value])

(defn reify-closure-hole
  [return plug]
  (reify-hole
   (fn [plug-outer plug-inner]
     (let [closure-symbol (gensym "c__")]
       (plug
        (fn [body-form]
          (return
           (fn [return outer-form]
             (plug-outer
              return
              (prepend-binding
               `(if (instance? ClosureResult ~closure-symbol)
                  ~body-form
                  ~closure-symbol)
               'let*
               closure-symbol
               outer-form)))
           (fn [return inner-form]
             (if *in-continuation?*
               (plug-inner return inner-form)
               (plug
                (fn [_]
                  (return `(ClosureResult. ~inner-form)))
                inner-form)))))
        `(.value ~closure-symbol))))
   plug))
