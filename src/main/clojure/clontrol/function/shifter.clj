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

(defmacro ^:private def-Shifter-protocol
  {:clj-kondo/lint-as 'clojure.core/defprotocol}
  [name-symbol]
  (let [this-symbol 'this
        return-symbol 'return
        invoke-symbol 'invoke-shift
        apply-symbol 'apply-shift]
    `(defprotocol ~name-symbol
       (~invoke-symbol
         ~@(for [parameter-symbols (make-parameter-arities shifter-arities)]
             `[~this-symbol ~return-symbol ~@parameter-symbols]))
       (~apply-symbol
         [~this-symbol ~return-symbol ~'ps]))))

(declare invoke-shift)

(def-Shifter-protocol
  Shifter)

(declare ^:private throw-out-of-prompt-exception)

(defmacro ^:private def-FnShift-type
  {:clj-kondo/lint-as 'clojure.core/deftype}
  [name-symbol]
  (let [handler-symbol
        (with-meta
          'run-with-continuation
          {:tag 'clojure.lang.IFn})]
    `(deftype ~name-symbol
         [~handler-symbol]
         Shifter
         ~@(for [parameter-symbols (make-parameter-arities shifter-arities)]
             `(invoke-shift
               [~'this ~'return ~@parameter-symbols]
               (~handler-symbol ~'return ~@parameter-symbols)))
         (apply-shift
           [~'this ~'return ~'ps]
           (. ~handler-symbol applyTo (list* ~'return ~'ps)))

         clojure.lang.IFn
         ~@(for [parameter-symbols (make-parameter-arities function-arities)]
             `(invoke
               [~'this ~@parameter-symbols]
               (throw-out-of-prompt-exception ~'this))))))

(def-FnShift-type
  FnShift)

(defn- throw-out-of-prompt-exception
  [^clontrol.function.shifter.FnShift this]
  (throw
   (ex-info
    (str this " can only be invoked within a continuation prompt.")
    {:handler (.run-with-continuation this)})))

(defmacro ^:private def-invoke-unknown-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [name-symbol]
  (let [shifter-class-symbol 'clontrol.function.shifter.Shifter
        function-symbol 'function
        shifter-symbol (vary-meta function-symbol assoc :tag shifter-class-symbol)
        return-symbol 'return]
    `(defn ~name-symbol
       ~@(for [parameter-symbols (make-parameter-arities (- shifter-arities 1))]
           `([~function-symbol ~return-symbol ~@parameter-symbols]
             (if (instance? ~shifter-class-symbol ~function-symbol)
               (. ~shifter-symbol invoke-shift ~return-symbol ~@parameter-symbols)
               (~return-symbol (~function-symbol ~@parameter-symbols)))))
       ~(let [fixed-parameter-symbols (vec (make-parameter-arity (- shifter-arities 1)))
              rest-symbol 'ps
              parameter-symbols (conj fixed-parameter-symbols '& rest-symbol)
              argument-form `(list* ~@fixed-parameter-symbols ~rest-symbol)]
          `([~function-symbol ~return-symbol ~@parameter-symbols]
            (if (instance? ~shifter-class-symbol ~function-symbol)
              (. ~shifter-symbol apply-shift ~return-symbol ~argument-form)
              (~return-symbol (~function-symbol ~argument-form))))))))

(def-invoke-unknown-fn
  invoke-unknown)
