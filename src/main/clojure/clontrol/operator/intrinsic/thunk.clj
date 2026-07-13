(ns clontrol.operator.intrinsic.thunk
  "Intrinsic [[thunk]] operator for converting loops to CPS."
  (:refer-clojure :exclude [trampoline]))

(definterface Thunk
  (step []))

(definline thunk?
  [value]
  `(instance? clontrol.operator.intrinsic.thunk.Thunk ~value))

(defmacro thunk
  "Given a `body` it defers its computation by creating the
  corresponding [[Thunk]]."
  [& body]
  `(reify Thunk
     (step [this] ~@body)

     java.lang.Object
     (toString [_] "THUNK")))

(defn inline-trampoline
  [value-form]
  (let [value-symbol
        (gensym "v__")
        thunk-symbol
        (with-meta value-symbol
          {:tag 'clontrol.operator.intrinsic.thunk.Thunk})]
    `(loop [~value-symbol (. clojure.lang.RT (box ~value-form))]
       (if (thunk? ~value-symbol)
         (recur (. ~thunk-symbol (step)))
         ~value-symbol))))

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  {:inline #'inline-trampoline}
  [value]
  (if (thunk? value)
    (recur (. ^clontrol.operator.intrinsic.thunk.Thunk value (step)))
    value))
