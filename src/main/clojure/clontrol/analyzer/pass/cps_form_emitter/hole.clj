(ns clontrol.analyzer.pass.cps-form-emitter.hole
  (:require
   [clontrol.analyzer.pass.form-builder
    :refer [prepend-binding]]))

(def plug-into-identity
  ^{:function-form `identity}
  (fn [return form _] (return form)))

(defn continuation-form->hole
  [continuation-form]
  (if (= continuation-form `identity)
    plug-into-identity
    ^{:function-form continuation-form}
    (fn [return form _]
      (return `(~continuation-form ~form)))))

(defn hole->continuation-form
  [return plug]
  (if-let [function-form (:function-form (meta plug))]
    (return function-form)
    (let [argument-symbol (gensym "x__")]
      (plug
       (fn [body-form]
         (return
          `(fn* ([~argument-symbol] ~body-form))))
       argument-symbol
       {:in-continuation? true}))))

(defn reify-hole
  "Reifies `plug` into a continuation form `phi` and returns two holes:

  - `plug-outer` declaring the function `phi`.
  - `plug-inner` yielding the result o the computation to `phi`.

  This is used to avoid the generation of redundant code for branching
  operations, for which multiple branches may yield to the same
  continuation (see [[emit-if]], [[emit-case]], [[emit-try]])."
  [return plug]
  (hole->continuation-form
   (fn [continuation-form]
     (if (symbol? continuation-form)
       (return plug-into-identity plug)
       (let [phi-symbol
             (gensym "p__")
             bind-outer
             (fn [return outer-form _]
               (return
                (prepend-binding
                 outer-form
                 'let*
                 phi-symbol
                 continuation-form)))
             bind-inner
             ^{:function-form phi-symbol}
             (fn [return inner-form _]
               (return `(~phi-symbol ~inner-form)))]
         (return bind-outer bind-inner))))
   plug))

(deftype ClosureResult
    [value])

(defn reify-closure-hole
  [return plug context]
  (reify-hole
   (fn [plug-outer plug-inner]
     (let [closure-symbol (gensym "c__")]
       (plug
        (fn [body-form]
          (return
           (fn [return outer-form context]
             (plug-outer
              return
              (prepend-binding
               `(if (instance? ClosureResult ~closure-symbol)
                  ~body-form
                  ~closure-symbol)
               'let*
               closure-symbol
               outer-form)
              context))
           (with-meta
             (fn [return inner-form context]
               (if (:in-continuation? context)
                 (plug-inner return inner-form context)
                 (plug
                  (fn [_]
                    (return `(ClosureResult. ~inner-form)))
                  inner-form
                  context)))
             (meta plug-inner))))
        `(.value ~closure-symbol)
        context)))
   plug))
