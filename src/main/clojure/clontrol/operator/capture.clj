(ns clontrol.operator.capture
  "Intrinsic [[Capture]] type representing a partial continuation. This type
  is functionally equivalent to a Continuation Monad.")

(definterface Capture
  (resume [return]))

(definline capture?
  [value]
  `(instance? clontrol.operator.capture.Capture ~value))

(defmacro capture
  [return & body]
  `(reify Capture
     (resume [this ~return]
       ~@body)

     java.lang.Object
     (toString [_] "CAPTURE")))

(defn bind
  [return value]
  (if (capture? value)
    (. ^clontrol.operator.capture.Capture value (resume return))
    (return value)))
