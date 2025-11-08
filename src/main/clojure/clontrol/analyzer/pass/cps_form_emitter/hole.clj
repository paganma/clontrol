(ns clontrol.analyzer.pass.cps-form-emitter.hole
  (:require
   [clontrol.analyzer.pass.form-builder
    :refer [prepend-binding]]))

(def plug-into-identity
  ^{:function-form `identity}
  (fn [return form _]
    (return form)))

(defn continuation-form->hole
  [continuation-form]
  (if (= continuation-form `identity)
    plug-into-identity
    ^{:function-form continuation-form}
    (fn [return form _]
      (return `(~continuation-form ~form)))))

(defn hole->continuation-form
  [return plug context]
  (if-let [function-form (:function-form (meta plug))]
    (return function-form)
    (let [argument-symbol (gensym "x__")]
      (plug
       (fn [body-form]
         (return
          `(fn* ([~argument-symbol] ~body-form))))
       argument-symbol
       (merge context {:in-continuation? true})))))

(defn reify-hole
  [return return-tail plug-tail context]
  (hole->continuation-form
   (fn [continuation-form]
     (if (symbol? continuation-form)
       (return return-tail plug-tail context)
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
          (fn [return intermediate-form _]
            (return `(~continuation-symbol ~intermediate-form)))
          context))))
   plug-tail
   context))

(deftype Result [value])

(defn isolate-hole
  [return return-tail plug-tail context]
  (let [continuation-symbol (gensym "p__")]
    (hole->continuation-form
     (fn [continuation-form]
       (return
        (fn [result-form]
          (let [result-symbol (gensym "i__")]
            (plug-tail
             (fn [tail-form]
               (return-tail
                (prepend-binding
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
                  result-form)
                 'let*
                 continuation-symbol
                 continuation-form)))
             result-symbol
             context)))
        ^{:function-form continuation-symbol}
        (fn [return intermediate-form context]
          (if (:in-continuation? context)
            (return `(~continuation-symbol ~intermediate-form))
            (return `(Result. ~intermediate-form))))
        context))
     plug-tail
     context)))
