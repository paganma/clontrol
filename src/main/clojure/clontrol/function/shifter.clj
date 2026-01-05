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
  [^clontrol.function.shifter.Shifter this]
  (throw
   (ex-info
    (str this " can only be invoked within a continuation prompt.")
    {:handler (. this handler)})))

(defn ^:private emit-invoke-shift-body
  [shifter-symbol handler-symbol return-symbol parameter-symbols]
  `(let [~handler-symbol (. ~shifter-symbol handler)]
     (. ~handler-symbol invoke ~return-symbol ~@parameter-symbols)))

(defn ^:private emit-invoke-shift-arity
  [shifter-symbol handler-symbol return-symbol arity-count]
  (let [parameter-symbols (make-parameter-arity arity-count)]
    `([~shifter-symbol ~return-symbol ~@parameter-symbols]
      ~(emit-invoke-shift-body
        shifter-symbol
        handler-symbol
        return-symbol
        parameter-symbols))))

(defn ^:private emit-invoke-shift-variadic-body
  [shifter-symbol handler-symbol return-symbol parameter-symbols rest-form]
  (let [argument-form `(list* ~return-symbol ~@parameter-symbols ~rest-form)]
    `(let [~handler-symbol (. ~shifter-symbol handler)]
       (. ~handler-symbol applyTo ~argument-form))))

(defn ^:private emit-invoke-shift-variadic-arity
  [shifter-symbol handler-symbol return-symbol]
  (let [parameter-symbols (vec (make-parameter-arity (- shifter-arities 1)))
        rest-symbol 'ps
        variadic-parameter-symbols (conj parameter-symbols '& rest-symbol)]
    `([~shifter-symbol ~return-symbol ~@variadic-parameter-symbols]
      ~(emit-invoke-shift-variadic-body
        shifter-symbol
        handler-symbol
        return-symbol
        parameter-symbols
        rest-symbol))))

(defmacro ^:private def-invoke-shift-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [shifter-symbol 's
        handler-symbol 'h
        return-symbol 'k]
    `(defn ~function-name
       ~@(for [arity-count (range (- shifter-arities 1))]
           (emit-invoke-shift-arity
            shifter-symbol
            handler-symbol
            return-symbol
            arity-count))
       ~(emit-invoke-shift-variadic-arity
         shifter-symbol
         handler-symbol
         return-symbol))))

(defn inline-invoke-shift
  ([shifter-form return-form & parameters]
   (let [shifter-symbol (gensym "s__")
         handler-symbol (gensym "h__")
         return-symbol (gensym "k__")]
     `(let [~shifter-symbol ~shifter-form
            ~return-symbol ~return-form]
        ~(if (>= (count parameters) shifter-arities)
           (emit-invoke-shift-variadic-body
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
  ^{:inline inline-invoke-shift}
  invoke-shift)

(defn ^:private emit-invoke-unknown-body
  [function-symbol handler-symbol return-symbol parameter-symbols]
  (let [shifter-symbol
        (with-meta function-symbol
          {:tag 'clontrol.function.shifter.Shifter})]
    `(if (shifter? ~function-symbol)
       (let [~handler-symbol (. ~shifter-symbol handler)]
         (. ~handler-symbol invoke ~return-symbol ~@parameter-symbols))
       (~return-symbol (~function-symbol ~@parameter-symbols)))))

(defn ^:private emit-invoke-unknown-arity
  [function-symbol handler-symbol return-symbol arity-count]
  (let [parameter-symbols (make-parameter-arity arity-count)]
    `([~function-symbol ~return-symbol ~@parameter-symbols]
      ~(emit-invoke-unknown-body
        function-symbol
        handler-symbol
        return-symbol
        parameter-symbols))))

(defn ^:private emit-invoke-unknown-variadic-body
  [function-symbol handler-symbol return-symbol parameter-symbols rest-form]
  (let [shifter-symbol
        (with-meta function-symbol
          {:tag 'clontrol.function.shifter.Shifter})
        argument-form
        `(list* ~return-symbol ~@parameter-symbols ~rest-form)]
    `(if (shifter? ~function-symbol)
       (let [~handler-symbol (. ~shifter-symbol handler)]
         (. ~handler-symbol applyTo ~return-symbol ~argument-form))
       (~return-symbol (. ~function-symbol applyTo ~argument-form)))))

(defn ^:private emit-invoke-unknown-variadic-arity
  [function-symbol handler-symbol return-symbol]
  (let [parameter-symbols (vec (make-parameter-arity (- shifter-arities 1)))
        rest-symbol 'ps
        variadic-parameter-symbols (conj parameter-symbols '& rest-symbol)]
    `([~function-symbol ~return-symbol ~@variadic-parameter-symbols]
      ~(emit-invoke-unknown-variadic-body
        function-symbol
        handler-symbol
        return-symbol
        parameter-symbols
        rest-symbol))))

(defmacro ^:private def-invoke-unknown-fn
  {:clj-kondo/lint-as 'clojure.core/declare}
  [function-name]
  (let [function-symbol 'f
        handler-symbol 'h
        return-symbol 'k]
    `(defn ~function-name
       ~@(for [arity-count (range (- shifter-arities 1))]
           (emit-invoke-unknown-arity
            function-symbol
            handler-symbol
            return-symbol
            arity-count))
       ~(emit-invoke-unknown-variadic-arity
         function-symbol
         handler-symbol
         return-symbol))))

(defn inline-invoke-unknown
  ([function-form return-form & parameters]
   (let [function-symbol (gensym "s__")
         handler-symbol (gensym "h__")
         return-symbol (gensym "k__")]
     `(let [~function-symbol ~function-form
            ~return-symbol ~return-form]
        ~(if (>= (count parameters) shifter-arities)
           (emit-invoke-unknown-variadic-body
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
  ^{:inline inline-invoke-unknown}
  invoke-unknown)
