(ns clontrol.operator.intrinsic.thunk
  "Intrinsic [[thunk]] operator for converting loops to CPS."
  (:refer-clojure :exclude [trampoline]))

(defprotocol Thunk
  (run [this]))

(definline thunk?
  [value]
  `(instance? clontrol.operator.intrinsic.thunk.Thunk ~value))

(defn make-thunk
  [run]
  (reify Thunk
    (run [_] (run))))

(defmacro thunk
  "Given a `body` it defers its computation by creating the
  corresponding [[IThunk]]."
  [& body]
  (let [this-symbol (gensym "_")]
    `(reify Thunk
       (run [~this-symbol] ~@body))))

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  [value]
  (if (thunk? value)
    (let [value' (.run ^clontrol.operator.intrinsic.thunk.Thunk value)]
      (recur value'))
    value))
