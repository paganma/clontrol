(ns clontrol.function.shifter
  "The interface for a shifter")

(def ^:private ^:const max-invoke-parameters 22)

(def ^:private ^:const max-function-parameters 20)

(defn ^:private make-parameter-symbols
  [parameters-count]
  (vec
   (for [i (range parameters-count)]
     (symbol (str "p" (+ i 1))))))

(declare ^:private throw-out-of-prompt-exception)

(defmacro ^:private def-Shifter-type
  {:clj-kondo/lint-as 'clojure.core/deftype}
  [type-name]
  (let [handler-symbol (vary-meta 'handler assoc :tag 'clojure.lang.IFn)
        this-symbol 'this]
    `(deftype ~type-name
         [~handler-symbol]

       clojure.lang.IFn
       ~@(for [parameters-count (range max-invoke-parameters)]
           (let [parameter-symbols (make-parameter-symbols parameters-count)]
             `(invoke
               [~this-symbol ~@parameter-symbols]
               (throw-out-of-prompt-exception ~this-symbol))))

       ~(let [rest-symbol 'ps]
          `(applyTo
            [~this-symbol ~rest-symbol]
            (throw-out-of-prompt-exception ~this-symbol))))))

(def-Shifter-type Shifter)

(defn ^:private throw-out-of-prompt-exception
  [^clontrol.function.shifter.Shifter this]
  (throw
   (ex-info
    (str this " can only be invoked within a continuation prompt.")
    {:handler (. this handler)})))

(definline shifter?
  [value]
  `(instance? clontrol.function.shifter.Shifter ~value))


;;;; * Shifter callers

(def ^:private max-shifter-parameters (- max-function-parameters 3))


;;;; ** invoke-shift

(defn ^:private emit-invoke-shift-body
  [shifter-symbol handler-symbol return-symbol parameter-forms]
  `(let [~handler-symbol (. ~shifter-symbol handler)]
     (. ~handler-symbol invoke ~return-symbol ~@parameter-forms)))

(defn ^:private emit-invoke-shift-arity
  [shifter-symbol handler-symbol return-symbol parameters-count]
  (let [parameter-symbols (make-parameter-symbols parameters-count)]
    `([~shifter-symbol ~return-symbol ~@parameter-symbols]
      ~(emit-invoke-shift-body
        shifter-symbol
        handler-symbol
        return-symbol
        parameter-symbols))))

(defn ^:private emit-variadic-invoke-shift-body
  [shifter-symbol handler-symbol return-symbol parameter-symbols rest-form]
  (let [argument-form `(list* ~return-symbol ~@parameter-symbols ~rest-form)]
    `(let [~handler-symbol (. ~shifter-symbol handler)]
       (. ~handler-symbol applyTo ~argument-form))))

(defn ^:private emit-variadic-invoke-shift-arity
  [shifter-symbol handler-symbol return-symbol]
  (let [parameter-symbols (make-parameter-symbols (- max-shifter-parameters 1))
        rest-symbol 'ps
        variadic-parameter-symbols (conj parameter-symbols '& rest-symbol)]
    `([~shifter-symbol ~return-symbol ~@variadic-parameter-symbols]
      ~(emit-variadic-invoke-shift-body
        shifter-symbol
        handler-symbol
        return-symbol
        parameter-symbols
        rest-symbol))))

(defmacro ^:private def-invoke-shift-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [shifter-symbol 'shifter
        handler-symbol 'handler
        return-symbol 'return]
    `(defn ~function-name
       ~@(for [parameters-count (range max-shifter-parameters)]
           (emit-invoke-shift-arity
            shifter-symbol
            handler-symbol
            return-symbol
            parameters-count))
       ~(emit-variadic-invoke-shift-arity
         shifter-symbol
         handler-symbol
         return-symbol))))

(defn inline-invoke-shift
  ([shifter-form return-form & parameters]
   (let [shifter-symbol (gensym "s__")
         handler-symbol (gensym "h__")
         return-symbol (gensym "k__")]
     `(let [~shifter-symbol (. clojure.lang.RT (box ~shifter-form))
            ~return-symbol ~return-form]
        ~(if (> (count parameters)
                max-shifter-parameters)
           (emit-variadic-invoke-shift-body
            shifter-symbol
            handler-symbol
            return-symbol
            parameters
            nil)
           (emit-invoke-shift-body
            shifter-symbol
            handler-symbol
            return-symbol
            parameters))))))

(def-invoke-shift-fn
  ^{:inline #'inline-invoke-shift}
  invoke-shift)


;;;; ** invoke-unknown

(defn ^:private emit-invoke-unknown-body
  [function-symbol handler-symbol return-symbol parameter-forms]
  (let [shifter-symbol
        (with-meta function-symbol
          {:tag 'clontrol.function.shifter.Shifter})]
    `(if (shifter? ~function-symbol)
       (let [~handler-symbol (. ~shifter-symbol handler)]
         (. ~handler-symbol invoke ~return-symbol ~@parameter-forms))
       (~return-symbol (~function-symbol ~@parameter-forms)))))

(defn ^:private emit-invoke-unknown-arity
  [function-symbol handler-symbol return-symbol parameters-count]
  (let [parameter-symbols (make-parameter-symbols parameters-count)]
    `([~function-symbol ~return-symbol ~@parameter-symbols]
      ~(emit-invoke-unknown-body
        function-symbol
        handler-symbol
        return-symbol
        parameter-symbols))))

(defn ^:private emit-variadic-invoke-unknown-body
  [function-symbol handler-symbol return-symbol parameter-forms rest-form]
  (let [shifter-symbol
        (with-meta function-symbol
          {:tag 'clontrol.function.shifter.Shifter})
        argument-form
        `(list* ~return-symbol ~@parameter-forms ~rest-form)]
    `(if (shifter? ~function-symbol)
       (let [~handler-symbol (. ~shifter-symbol handler)]
         (. ~handler-symbol applyTo ~return-symbol ~argument-form))
       (~return-symbol (. ~function-symbol applyTo ~argument-form)))))

(defn ^:private emit-variadic-invoke-unknown-arity
  [function-symbol handler-symbol return-symbol]
  (let [parameter-symbols (make-parameter-symbols max-shifter-parameters)
        rest-symbol 'ps
        variadic-parameter-symbols (conj parameter-symbols '& rest-symbol)]
    `([~function-symbol ~return-symbol ~@variadic-parameter-symbols]
      ~(emit-variadic-invoke-unknown-body
        function-symbol
        handler-symbol
        return-symbol
        parameter-symbols
        rest-symbol))))

(defmacro ^:private def-invoke-unknown-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [function-symbol 'function
        handler-symbol 'handler
        return-symbol 'return]
    `(defn ~function-name
       ~@(for [parameters-count (range max-shifter-parameters)]
           (emit-invoke-unknown-arity
            function-symbol
            handler-symbol
            return-symbol
            parameters-count))
       ~(emit-variadic-invoke-unknown-arity
         function-symbol
         handler-symbol
         return-symbol))))

(defn inline-invoke-unknown
  ([function-form return-form & parameters]
   (let [function-symbol (gensym "s__")
         handler-symbol (gensym "h__")
         return-symbol (gensym "k__")]
     `(let [~function-symbol (. clojure.lang.RT (box ~function-form))
            ~return-symbol ~return-form]
        ~(if (> (count parameters)
                max-shifter-parameters)
           (emit-variadic-invoke-unknown-body
            function-symbol
            handler-symbol
            return-symbol
            parameters
            nil)
           (emit-invoke-unknown-body
            function-symbol
            handler-symbol
            return-symbol
            parameters))))))

(def-invoke-unknown-fn
  ^{:inline #'inline-invoke-unknown}
  invoke-unknown)
