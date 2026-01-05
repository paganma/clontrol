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
       (assoc context :in-continuation? true)))))

(defn capture-hole
  [return return-tail plug context]
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
          (fn [return intermediate-form _]
            (return `(~continuation-symbol ~intermediate-form)))))))
   plug
   context))

(deftype IntermediateResult [value])

(defn capture-recur-hole
  [return return-tail plug context]
  (hole->continuation-form
   (fn [continuation-form]
     (let [continuation-symbol (gensym "p__")
           result-symbol (gensym "i__")]
       (return
        (fn [intermediate-form]
          (plug
           (fn [tail-form]
             (return-tail
              (-> `(if (instance? IntermediateResult ~result-symbol)
                     ~(prepend-binding
                       tail-form
                       'let*
                       result-symbol
                       `(.value
                         ~(with-meta result-symbol
                            {:tag IntermediateResult})))
                     ~result-symbol)
                  (prepend-binding
                   'let*
                   result-symbol
                   intermediate-form)
                  (prepend-binding
                   'let*
                   continuation-symbol
                   continuation-form))))
           result-symbol
           context))
        ^{:function-form continuation-symbol}
        (fn [return intermediate-form context]
          (if (:in-continuation? context)
            (return `(~continuation-symbol ~intermediate-form))
            (return `(IntermediateResult. ~intermediate-form)))))))
   plug
   context))
