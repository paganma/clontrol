(ns clontrol.analyzer.pass.cps-form-emitter.hole
  (:require
   [clontrol.analyzer.pass.form-builder
    :refer [prepend-binding]]))

(def plug-into-identity
  ^{:function-form `identity}
  (fn [return form]
    (return form)))

(defn continuation-form->hole
  [continuation-form]
  (if (= continuation-form `identity)
    plug-into-identity
    ^{:function-form continuation-form}
    (fn [return form]
      (return `(~continuation-form ~form)))))

(defn hole->continuation-form
  [return plug]
  (if-let [function-form (:function-form (meta plug))]
    (return function-form)
    (let [argument-symbol (gensym "x__")]
      (plug
       (fn [body-form]
         (return `(fn* ([~argument-symbol] ~body-form))))
       argument-symbol))))

(defn capture-hole
  [return return-tail plug]
  (hole->continuation-form
   (fn [continuation-form]
     (if (symbol? continuation-form)
       (return return-tail plug)
       (let [continuation-symbol (gensym "p__")]
         (return
          (fn [tail-form]
            (return-tail
             (prepend-binding
              tail-form
              'let*
              continuation-symbol
              continuation-form)))
          ^{:function-form continuation-symbol}
          (fn [return intermediate-form]
            (return `(~continuation-symbol ~intermediate-form)))))))
   plug))
