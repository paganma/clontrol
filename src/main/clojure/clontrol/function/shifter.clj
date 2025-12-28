(ns clontrol.function.shifter
  "The interface for a shifter")

(def ^:private ^:const shifter-arities 18)

(def ^:private ^:const function-arities 21)

(defn ^:private make-parameter-arity
  [parameters-count]
  (for [i (range parameters-count)]
    (symbol (str "p" i))))

(defn ^:private make-parameter-arities
  [arities-count]
  (for [i (range arities-count)]
    (make-parameter-arity i)))

(declare ^:private throw-out-of-prompt-exception)

(defmacro ^:private def-Shifter-type
  {:clj-kondo/lint-as 'clojure.core/deftype}
  [type-name]
  (let [handler-symbol (vary-meta 'handler assoc :tag 'clojure.lang.IFn)
        this-symbol (gensym "this__")]
    `(deftype ~type-name
         [~handler-symbol]
         clojure.lang.IFn
         ~@(for [parameter-symbols (make-parameter-arities function-arities)]
             `(invoke
               [~this-symbol ~@parameter-symbols]
               (throw-out-of-prompt-exception ~this-symbol))))))

(def-Shifter-type Shifter)

(definline shifter?
  [value]
  `(instance? clontrol.function.shifter.Shifter ~value))

(defn- throw-out-of-prompt-exception
  [^Shifter this]
  (throw
   (ex-info
    (str this " can only be invoked within a continuation prompt.")
    {:handler (.handler this)})))

(defmacro ^:private def-invoke-shift-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [shifter-class-symbol 'clontrol.function.shifter.Shifter
        shifter-symbol (vary-meta 'function assoc :tag shifter-class-symbol)
        handler-symbol (vary-meta 'handler assoc :tag 'clojure.lang.IFn)
        return-symbol 'k]
    `(defn ~function-name
       ~@(for [parameter-symbols (make-parameter-arities (- shifter-arities 1))]
           `([~shifter-symbol ~return-symbol ~@parameter-symbols]
             (let [~handler-symbol (. ^Shifter ~shifter-symbol handler)]
               (. ~handler-symbol invoke ~return-symbol ~@parameter-symbols))))
       ~(let [fixed-parameter-symbols (vec (make-parameter-arity (- shifter-arities 1)))
              rest-symbol 'ps
              parameter-symbols (conj fixed-parameter-symbols '& rest-symbol)
              argument-form `(list* ~@fixed-parameter-symbols ~rest-symbol)]
          `([~shifter-symbol ~return-symbol ~@parameter-symbols]
            (let [~handler-symbol (. ^Shifter ~shifter-symbol handler)]
              (. ~handler-symbol applyTo (list* ~return-symbol ~argument-form))))))))

(def-invoke-shift-fn invoke-shift)

(defmacro ^:private def-invoke-unknown-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [shifter-class-symbol 'clontrol.function.shifter.Shifter
        function-symbol 'function
        shifter-symbol (vary-meta function-symbol assoc :tag shifter-class-symbol)
        handler-symbol (vary-meta 'handler assoc :tag 'clojure.lang.IFn)
        return-symbol 'k]
    `(defn ~function-name
       ~@(for [parameter-symbols (make-parameter-arities (- shifter-arities 1))]
           `([~function-symbol ~return-symbol ~@parameter-symbols]
             (if (shifter? ~function-symbol)
               (let [~handler-symbol (. ^Shifter ~shifter-symbol handler)]
                 (. ~handler-symbol invoke ~return-symbol ~@parameter-symbols))
               (~return-symbol (~function-symbol ~@parameter-symbols)))))
       ~(let [fixed-parameter-symbols (vec (make-parameter-arity (- shifter-arities 1)))
              rest-symbol 'ps
              parameter-symbols (conj fixed-parameter-symbols '& rest-symbol)
              argument-form `(list* ~@fixed-parameter-symbols ~rest-symbol)]
          `([~function-symbol ~return-symbol ~@parameter-symbols]
            (if (shifter? ~function-symbol)
              (let [~handler-symbol (. ^Shifter ~shifter-symbol handler)]
                (. ~handler-symbol applyTo ~return-symbol (list* ~return-symbol ~argument-form)))
              (~return-symbol (~function-symbol ~argument-form))))))))

(def-invoke-unknown-fn invoke-unknown)
