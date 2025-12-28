# Clontrol: Delimited continuations for Clojure on the JVM

A delimited continuation library providing an implementation of Danvy and
Filinski's `shift` and `reset` control operators, along with the facilities to
make programming with continuations more practical.

## Usage

The delimited control operators allow you to `shift` the control within the
nearest enclosing `reset` block reifying the rest of the computation of the
block into a function `k`. The function `k` will then bind its argument to the
`shift` position. Considering the following simple examples:
```clojure
(require '[clontrol.core :refer [shift reset]])

;; Binds the rest of the computation to 1. This is effectively
;; equivalent to (+ 1 1).
(reset
 (+ 1 (shift (fn [k] (k 1))))) ;; => 2

;; We can call the rest of the computation as many times as we
;; want. In this case we call k with three arguments and combine the
;; outcomes within a vector.
(reset
 (+ 1 (shift (fn [k] [(k 1) (k 2) (k 3)])))) ;; => [2 3 4]

;; A reset block may also contain multiple shift operations.
(reset
 (+ (shift (fn [k] [(k 1) (k 2)]))
    (shift (fn [k] [(k 1) (k 2)])))) ;; => [[2 3] [3 4]]

;; We can also not call k at all.
(reset
 (+ 1 (shift (fn [_] 0)))) ;; => 0

```

Using these operators we can implement new programming constructs. The following
example shows how to write a simple implementation of a generator yielding a
sequence of number by capturing the continuation in the iteration of a loop.
```clojure
(require '[clontrol.core :refer [shift reset]])

;; The generator
(def g
  (atom
   (reset
     (loop [x 0]
       ;; Defer the next iteration and yield (inc x)
       (recur (shift (fn [k] [k (inc x)])))))))

;; Getter for the yielded value
(defn read [g]
  (second @g))

;; Continues to the next iteration
(defn step [g]
  (let [k (first @g)
        v (second @g)]
    (reset! g (k v))
    g))

(read (step g)) ;; => 1
(read (step g)) ;; => 2
(read (step g)) ;; => 3
;; ...
```

### Continuations across function calls

The `shift` operator can only be used within the executable scope of a `reset`
block, hence:
```clojure
(require '[clontrol.core :refer [shift reset]])

(defn divide [x y]
  (if (= y 0)
    (shift (fn [_] 'NaN)) ;;; ERROR: Must be called within a continuation prompt.
    (/ x y)))

(reset
  (divide 10 0))
```

will raise a macroexpansion error on the `shift` invocation.

To allow `shift` to capture the continuation of the function's caller it must be
placed in Clontrol's special function forms `fn-shift` (for defining lambdas) or
`defn-shift` (for declared functions):

```clojure
(require '[clontrol.core :refer [shift reset defn-shift]])

(defn-shift divide [x y]
  (if (= y 0)
    (shift (fn [_] 'NaN))
    (/ x y)))

(reset
  (divide 10 0))
```

Note that functions declared with `fn-shift` can only be invoked either within a
continuation prompt (e.g. a `reset` block), or within the body other `fn-shift`
functions.

## Status

This project is still experimental. More testing needs to be done to ensure that
the selective CPS transformation does not introduce unexpected behaviors.

## License

Copyright © 2025 Marco Paganoni

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
