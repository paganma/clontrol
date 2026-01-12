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

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  [value]
  (if (thunk? value)
    (let [continue (. ^clontrol.operator.intrinsic.thunk.Thunk value continue)]
      (recur (continue)))
    value))
