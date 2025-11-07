(ns clontrol.analyzer.pass.cps-form-emitter.hole)

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
