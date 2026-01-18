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

(defn inline-step
  [thunk-form]
  (let [thunk-form
        (with-meta thunk-form
          {:tag 'clontrol.operator.intrinsic.thunk.Thunk})]
    `((. ~thunk-form continue))))

(defn step
  [^clontrol.operator.intrinsic.thunk.Thunk thunk]
  ((. thunk continue)))

(defn inline-trampoline
  [value-form]
  (let [value-symbol (gensym "v__")]
    `(let [~value-symbol ~value-form]
       (loop [~value-symbol ~value-symbol]
         (if (thunk? ~value-symbol)
           (recur (step ~value-symbol))
           ~value-symbol)))))

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  [value]
  (if (thunk? value)
    (let [continue (. ^clontrol.operator.intrinsic.thunk.Thunk value continue)]
      (recur (continue)))
    value))
