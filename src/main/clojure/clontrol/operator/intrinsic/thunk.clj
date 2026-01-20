(ns clontrol.operator.intrinsic.thunk
  "Intrinsic [[thunk]] operator for converting loops to CPS."
  (:refer-clojure :exclude [trampoline]))

(deftype Thunk
  [continue])

(definline thunk?
  [value]
  `(instance? clontrol.operator.intrinsic.thunk.Thunk ~value))

(defmacro thunk
  "Given a `body` it defers its computation by creating the
  corresponding [[Thunk]]."
  [& body]
  `(Thunk. (fn [] ~@body)))

(defn inline-trampoline
  [value-form]
  (let [value-symbol
        (gensym "v__")
        thunk-symbol
        (with-meta value-symbol
          {:tag 'clontrol.operator.intrinsic.thunk.Thunk})]
    `(let [~value-symbol (. clojure.lang.RT (box ~value-form))]
       (loop [~value-symbol ~value-symbol]
         (if (thunk? ~value-symbol)
           (recur ((. ~thunk-symbol continue)))
           ~value-symbol)))))

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  {:inline #'inline-trampoline}
  [value]
  (if (thunk? value)
    (let [continue (. ^clontrol.operator.intrinsic.thunk.Thunk value continue)]
      (recur (continue)))
    value))
