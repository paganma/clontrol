(ns clontrol.analyzer.pass.cps-form-emitter.hole
  (:require
   [clontrol.analyzer.pass.form-builder
    :refer [prepend-binding
            build-recur-tail]]))

(def plug-into-identity
  ^{:function-form `identity}
  (fn [return form] (return form)))

(defn continuation-form->hole
  [return function-form]
  (return
   (if (= function-form `identity)
     plug-into-identity
     ^{:function-form function-form}
     (fn [return form]
       (return `(~function-form ~form))))))

(defn hole->continuation-form
  [return plug]
  (if-let [function-form (:function-form (meta plug))]
    (return function-form)
    (let [argument-symbol (gensym "x__")
          body-form (trampoline plug identity argument-symbol)
          body-form (build-recur-tail body-form :build-indirect)]
      (return `(fn* ([~argument-symbol] ~body-form))))))

(defn reify-hole
  "Reifies `plug` into a continuation form `phi` and returns two holes:

  - `plug-outer` declaring the function `phi`.
  - `plug-inner` yielding the result o the computation to `phi`.

  This is used to avoid the generation of redundant code for branching
  operations, for which multiple branches may yield to the same
  continuation (see [[emit-if]], [[emit-case]], [[emit-try]])."
  [return plug]
  (hole->continuation-form
   (fn [function-form]
     (if (symbol? function-form)
       (return plug-into-identity plug)
       (let [phi-symbol
             (gensym "p__")
             bind-outer
             (fn [return outer-form]
               (return (prepend-binding outer-form 'let* phi-symbol function-form)))
             bind-inner
             (fn [return inner-form]
               (return `(~phi-symbol ~inner-form)))]
         (return bind-outer bind-inner))))
   plug))
