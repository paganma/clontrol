(ns clontrol.operator.thunk
  (:refer-clojure :exclude [trampoline]))

(deftype Thunk
         [^clojure.lang.Fn run-thunk]

  clojure.lang.IFn
  (invoke [_] (run-thunk)))

(defn thunk?
  [x]
  {:post
   [(boolean? %)]}
  (instance? clontrol.operator.thunk.Thunk x))

(defmacro thunk
  "Given a `body` it defers its computation by creating the
  corresponding [[IThunk]]."
  [& body]
  `(Thunk. (^:once fn* [] ~@body)))

(defn make-thunk
  [run-thunk]
  {:pre
   [(ifn? run-thunk)]
   :post
   [(thunk? %)]}
  (->Thunk run-thunk))

(defn trampoline
  "Recursively expands `x` if it is a [[Thunk]] instance. Analogous
  to [[clojure.core/trampoline]] but for [[Thunk]] instances instead
  of plain functions."
  [value]
  (if (instance? clontrol.operator.thunk.Thunk value)
    (let [x' (^clojure.lang.Fn (.run-thunk ^Thunk value))]
      (recur x'))
    value))

(defn lift
  [continuation]
  (println continuation)
  (fn [x]
    (println x)
    (trampoline (continuation x))))
