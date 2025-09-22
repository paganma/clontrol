(ns clontrol.analyzer.pass.form-builder
  "Utilities for constructing forms.")

(defmacro construct-bindings
  "Given the symbol of the lexical closure `head` (e.g. `let*`), a list of
  `bindings` and a `body`, it expands to: `(head [~@bindings] ~body)`"
  [head bindings body]
  (with-meta
    (list head (vec bindings) body)
    (meta &form)))

(defn prepend-binding
  "Binds `binding-symbol` to `value-form` in a lexical closure
  `head-symbol` (e.g. `'let*`)."
  [form head-symbol binding-symbol value-form]
  (with-meta 
    (if (and (seq? form)
             (= (first form) `construct-bindings)
             (= (second form) head-symbol))
      (let [[_ _ binding-forms form] form
            binding-symbols' (conj binding-forms value-form binding-symbol)]
        `(construct-bindings ~head-symbol ~binding-symbols' ~form))
      `(construct-bindings ~head-symbol (~binding-symbol ~value-form) ~form))
    (meta form)))

(defmacro construct-statements
  "Given a list of `statements` it expands to: `(do ~@statements)`"
  [statements]
  (with-meta
    (list* 'do statements)
    (meta &form)))

(defn prepend-statement
  "Prepends a `statement-form` to `form`."
  [form statement-form]
  (with-meta
    (if (and (seq? form)
             (= (first form) `construct-statements))
      (let [[_ statement-forms] form
            statement-forms' (conj statement-forms statement-form)]
        `(construct-statements ~statement-forms'))
      `(construct-statements (~statement-form ~form)))
    (meta form)))

(defrecord RecurTailBuilder
    [build-direct build-indirect])

(defn build-recur-tail
  [value accessor]
  (loop [value value]
    (if (instance? RecurTailBuilder value)
      (recur (trampoline (accessor value)))
      value)))
