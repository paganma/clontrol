(ns clontrol.operator-test
  (:require
   [clojure.test
    :refer [deftest is testing]]
   [clontrol.function
    :refer [fn-shift]]
   [clontrol.operator
    :refer [abort
            reset
            reset-to
            shift
            shift*]]))

(set! *warn-on-reflection* false)

(deftest intrinsic-operator-test
  (is (thrown? java.lang.Exception (macroexpand `(shift* identity)))))

;;;; * Operator tests

(defn nop [])

(defn nop-1 [_])

(deftest operator-test

  (testing "Generator"
    (letfn [(yield [k x] [k x])
            (step [[k x]] (k x))
            (fetch [[_ x]] x)]
      (let [generator
            (reset
             (loop [a 0]
               (shift yield a)
               (recur (+ a 1))))]
        (is (= (fetch (step generator)) 1))
        (is (= (fetch (step (step generator))) 2)))))

  (testing "CC Generator"
    (let [yield
          (fn-shift
           [x]
           (shift (fn [k] [k x])))]
      (letfn
       [(step [[k x]] (k x))
        (fetch [[_ x]] x)]
        (let [generator
              (reset
               (loop [a 0]
                 (yield a)
                 (recur (+ a 1))))]
          (is (= (fetch (step generator)) 1))
          (is (= (fetch (step (step generator))) 2))))))

  (testing "Stateful Generator"
    (letfn [(yield [k x]
              (reset-to k (shift (fn [k] [k x]))))]
      (let [fetch (fn [t] (second @t))
            step (fn [t] (reset! t ((first @t) (second @t))))
            generator
            (atom
             (reset
              (loop [x 0]
                (recur (shift yield (inc x))))))]
        (dotimes [_ 10000]
          (step generator))
        (is (= (fetch generator) 10001)))))

  (testing "Stateful Generator with Handlers"
    (let [yield (fn-shift [x] (shift (fn [k] [k x])))
          fetch (fn [t] (second @t))
          step (fn [t] (reset! t ((first @t) (second @t))))
          generator
          (atom
           (reset
            (loop [x 0]
              (yield (+ x 1))
              (recur (inc x)))))]
      (dotimes [_ 10000]
        (step generator))
      (is (= (fetch generator) 10001))))

  (testing "Direct recur in CPS loop"
    (reset
     (loop []
       (if true
         (shift (fn [k] (k 2)))
         (recur)))))

  (testing "Direct recur in direct loop"
    (reset
     (loop []
       (when false
         (recur)))))

  (testing "CPS recur in CPS loop"
    (reset
     (loop [a 1]
       (when (< a 10)
         (recur (shift (fn [k] (k (+ a 1)))))))))

  (testing "CPS recur in CPS loop"
    (reset
     (loop [a 1]
       (when (< a 10)
         (recur (shift (fn [k] (k (+ a 1)))))))))

  (testing "CPS loop bindings"
    (reset
     (loop [a (shift (fn [k] (k 1)))]
       (when (< a 10)
         (recur (+ a 1))))))

  (testing "Shift in Simple Expressions"
    (is (= (reset (shift (fn [k] (k 1))))
           1))

    (is (= (reset (shift (fn [_] nil)))
           nil))

    (is (= (reset
            (+
             (shift (fn [k] [(k 1) (k 2)]))
             (shift (fn [k] [(k 1) (k 2)]))
             (shift (fn [k] [(k 1) (k 2)]))))
           [[[3 4] [4 5]] [[4 5] [5 6]]]))

    (is (= (reset
            (do
              (shift (fn [k] (+ (k nil) (k nil))))
              (+ 4 4)))
           16))

    (is (= (reset (+ 2 3 (shift (fn [k] (+ (k 1) (k 2) 3))))) 16)))

  (testing "Composability"
    (letfn
     [(divide [k a b]
        (reset-to k
                  (if (= b 0)
                    (shift (fn [_] 'NaN))
                    (/ a b))))

      (fib [k n]
        (reset-to k
                  (if (<= n 2)
                    1
                    (+ (shift fib (- n 1)) (shift fib (- n 2))))))]

      (is (= (reset (+ 1 2 (shift divide 3 0))) 'NaN))

      (is (= (reset (+ 1 2 (shift divide 3 3))) 4))

      (is (= (reset (+ 1 (shift (fn [k] (+ (k 1) (k 2)))) (shift divide 3 3))) 7))

      (is (= (reset (shift fib 10)) 55))))

  (testing "Composability"
    (is (let [a
              (fn-shift
               [x]
               (shift (fn [k] [(k 1) (k 2)])))]
          (= (reset (a 1)) [1 2]))))

  (testing "Shift in CASE"
    (is (= (reset
            (case (shift (fn [k] (k 1)))
              1 1
              0 0))
           1))

    (is (= (reset
            (case (shift (fn [k] [(k 0) (k 1)]))
              1 1
              0 0))
           [0 1]))

    (is (= (reset
            (case 1
              1 (shift (fn [k] (k 2)))
              0 0))
           2))

    (is (= (reset
            (case 1
              1 2
              0 (shift (fn [k] (k 3))))
            3)
           3)))

  #_{:clj-kondo/ignore [:inline-def]}
  (testing "Shift in DEF"
    #_{:clj-kondo/ignore [:uninitialized-var]}
    (is (var? (reset (def t) #'t)))

    (is (= (reset
            (def t (shift (fn [k] (k 1))))
            t)
           1))

    (is (reset
          (def ^{:test (shift (fn [k] (k true)))} t))
        #'clontrol.operator-test/t)

    (is (= (reset
            (def t (shift (fn [k] [(k 1) (k 2)])))
            (def d (+ 1 t))
            d)
           [2 3]))

    (is (= (reset
            (def a 1)
            (def b (+ a 1))
            (def c (shift (fn [_] (+ b 1))))
            c)
           3))

    (is (= (:a (meta (reset (def ^{:a (shift (fn [k] (k 1)))} t 1)))) 1))

    (is (= (reset
            (def ^{:a (shift (fn [k] [(k 1) (k 2)]))} f 1)
            (:a (meta #'f)))
           [1 2])))

  (testing "Shift in HOST-INTEROP"
    (is (= (let [a (java.awt.Point. 0 1)
                 b (java.awt.Point. 0 2)]
             (reset
              (. (shift (fn [k] [(k a) (k b)])) -y)))
           [1 2]))
    (is (= (reset
            (let [a (java.awt.Point. 0 (shift (fn [k] [(k 1) (k 2)])))
                  b (java.awt.Point. 0 2)]
              (. (shift (fn [k] [(k a) (k b)])) -y)))
           [[1 2] [2 2]]))
    (is (= (let [b (java.awt.Point. 0 1)]
             (reset
              (loop [a 10]
                (if (<= a 0)
                  a
                  (recur (- a (. (shift (fn [k] (k b))) -y)))))))
           0)))

  (testing "Shift in Sequence Literals"
    (is (= (reset (list 1 2 (shift (fn [k] (k 1))))) (list 1 2 1)))

    (is (= (reset #{1 2 3 (shift (fn [_] 1))}) 1))

    (is (= (reset #{1 2 3 (shift (fn [k] (k 4)))}) #{1 2 3 4}))

    (is (= (reset [1 2 3 4 (shift (fn [k] (k 5)))]) [1 2 3 4 5]))

    (is (= (reset {(shift (fn [k] [(k :a) (k :b)])) (shift (fn [k] [(k 1) (k 2)]))})
           [[{:a 1} {:a 2}] [{:b 1} {:b 2}]])))

  (testing "Shift in SET!"
    (is (= (do
             (def ^:dynamic *t*)
             (binding [*t* 0]
               (reset
                 (set! *t* (shift (fn [k] [(k 1) (k 2) (k 3)])))
                 *t*)))
           [1 2 3]))
    (is (= (let [p (java.awt.Point.)]
             (reset
              (set! (. p -y) (shift (fn [k] [(k 1) (k 2) (k 3)])))
              (. p -y)))
           [1 2 3]))
    (is (= (let [a (java.awt.Point. 0 1)
                 b (java.awt.Point. 0 2)]
             (reset
              (set! (. (shift (fn [k] [(k a) (k b)])) -y) 3)))
           [3 3]))
    (is (= (let [a (java.awt.Point. 0 1)
                 b (java.awt.Point. 0 2)]
             (reset
              (set! (. ^java.awt.Point (shift (fn [k] [(k a) (k b)])) -y) 3)))
           [3 3]))
    (is (= (let [a (java.awt.Point. 0 1)]
             (reset
              (set! (. a -y) (shift (fn [k] [(k 1) (k 2)])))))
           [1 2])))

  (testing "Shift in IF"
    (is (= (reset
            (if (shift (fn [k] [(k true) (k false)]))
              1
              2))
           [1 2]))
    (is (= (reset
            (if true
              1
              (shift (fn [k] (k 2)))))
           1))
    (is (= (reset
            (if true
              (shift (fn [k] (k 2)))
              1))
           2))
    (is (= (reset
            (if false
              1
              (shift (fn [k] (k 2)))))
           2))
    (is (= (reset
            (if true
              1
              (shift (fn [k] (k 2)))))
           1)))

  (testing "Shift in INSTANCE?"
    (is (= (reset
            (instance? java.lang.Object (shift (fn [k] [(k nil) (k '())]))))
           [false true]))
    (is (= (reset
            (instance? (shift (fn [k] [(k java.lang.Object) (k java.lang.String)])) '()))
           [true false])))

  (testing "Shift in NEW"
    (is (= (reset
            (java.awt.Point. (shift (fn [k] (k 0))) 0))
           (java.awt.Point. 0 0)))

    (is (= (reset
            (java.awt.Point. (shift (fn [k] [(k 0) (k 1)])) 0))
           [(java.awt.Point. 0 0) (java.awt.Point. 1 0)])))

  (testing "Shift in LOOP"
    (testing "Shift in LOOP BODY"
      (is (= (reset
              (loop [a 0]
                (when (> a 0)
                  (recur 1))))
             nil))

      (is (= (reset
              (loop [a 0]
                (if (< a (shift (fn [k] (k 10))))
                  (recur (+ a 1))
                  a)))
             10))

      (is (= (reset
              (loop [a 0]
                (if (< a (shift (fn [k] (k 10))))
                  (recur (+ a 1))
                  a)))
             10))

      (is (= (reset
              (loop [a 0]
                (if (< a 10)
                  (recur (shift (fn [k] (k (+ a 1)))))
                  a)))
             10))

      (is (= (reset
              (loop [a 1]
                (if (< a (shift (fn [k] [(k 1) (k 2)])))
                  (recur (+ a 1))
                  a)))
             [1 [2 2]]))

      (is (nil?
           ((reset
             (let [a 10]
               (dotimes [i a]
                 (when (< i 1)
                   (shift identity))))) nil)))

      (is (= (reset
              (loop [a 0]
                (when (= a 5)
                  (shift (fn [_] 1)))
                (recur (+ a 1))))
             1))

      (is (= (reset
              (loop [_ 0]
                (recur
                 (loop [a 0]
                   (when (= a 5)
                     (abort 1))
                   (recur (+ a 1)))))) 1))

      (is (= (reset
              (loop [a 0]
                (loop [b 0]
                  (if (< b 10)
                    (do
                      (if (> a 5)
                        (abort a))
                      (recur (inc b)))
                    b))
                (if (< a 10)
                  (recur (inc a))
                  a)))
             6))

      (is (= (reset
              (loop [a 0]
                (loop [b 0]
                  (if (< b 10)
                    (do
                      (if (> a 10)
                        (abort a))
                      (recur (inc b)))
                    b))
                (if (< a 10)
                  (do
                    (if (> a 5)
                      (abort a))
                    (recur (inc a)))
                  a)))
             6))

      (is (= (reset
              (loop [a 0]
                (loop [b 0]
                  (if (< b 10)
                    (recur (inc b))
                    b))
                (if (< a 10)
                  (do
                    (if (> a 5)
                      (abort a))
                    (recur (inc a)))
                  a)))
             6))

      (is (= (reset
              (loop [a 0]
                (when (= a 5)
                  (shift (fn [_] 1)))
                (recur (+ (shift (fn [k] (k a))) 1))))
             1)))

    (testing "Shift in LOOP BINDINGS"
      #_{:clj-kondo/ignore [:loop-without-recur]}
      (is (= (reset
              (loop [a (shift (fn [k] (k 1)))]
                a))
             1))
      (is (= (reset
              (loop [a (shift (fn [k] (k 1)))]
                (if (< a 10)
                  (recur (inc a))
                  a)))
             10))
      (is (= (reset
              (loop [a (shift (fn [k] (k 1)))]
                (if (< a 10)
                  (recur (shift (fn [k] (k (inc a)))))
                  a)))
             10))))

  (testing "Shift in DO"
    (is (= (reset
            (do
              (nop)
              (+ 1 (shift (fn [k] (k 1))))))
           2))

    (is (= (reset
            (do
              (nop-1 (shift (fn [k] (k 1))))
              (nop)
              (+ 1 (shift (fn [k] (k 2))))))
           3))

    (is (= (reset
            (with-local-vars
             [a 0
              b 0
              c 0]
              (var-set a (+ @a 1))
              (var-set b (shift (fn [k]
                                  (if (>= @a 0)
                                    (k (- @a 1))
                                    (k (+ @a 1))))))
              (var-set c (+ @a @b))))
           1))

    (is (= (reset
            (with-local-vars
             [a 0
              b 0
              c 0]
              (reset
               (loop []
                 (if (< @a 10)
                   (do (var-set a (+ @a 1)) (recur))
                   (shift (fn [_] nil)))))
              (var-set c (+ @a @b))))
           10)))

  (testing "Shift in LET"
    (is (= (reset
            (let [a 1
                  b 2]
              (+ (shift (fn [k] (+ (k a) (k b)))) 1)))
           5))

    (is (= (reset
            (let [a 1
                  b (+ (shift (fn [k] (k (+ a 1)))) 1)]
              (+ a b)))
           4))

    (is (= (reset
            (let [a 1
                  b (shift (fn [k] (+ (k a) 1)))
                  c (shift (fn [k] (+ (k b) 1)))]
              c))
           3)))

  (testing "Shift in LETFN"
    (is (= (reset
            (letfn
             [(f [x] x)
              (g [y] y)]
              (+ (shift
                  (fn [k]
                    (+ (f (k 1)) (g (k 2)))))
                 1)))
           5)))

  (testing "Shift in INVOKE.KEYWORD"
    (is (= (reset
            (:a {:a (shift (fn [k] [(k 1) (k 2)]))}))
           [1 2]))
    (is (= (reset
            (:a {(shift (fn [k] [(k :a) (k :b)])) 1}))
           [1 nil])))

  (testing "Shift in TRY-CATCH"
    (is (reset
         (try
           (throw (ex-info "" {}))
           false
           (catch Exception _
             (shift (fn [_] true))))))
    (is (reset
         (try
           (shift (fn [_] true))
           false
           (catch Exception _
             false))))
    (is (reset
         (try
           (throw (ex-info "" {:msg (shift (fn [_] true))}))
           false
           (catch Exception _
             false))))
    (is (thrown? Exception
                 (reset
                   (try
                     (shift (fn [k] (k nil)))
                     (catch Exception _
                       false))
                   (throw (ex-info "test" {})))))
    (is (reset
          (let [a (atom false)]
            (try
              (try
                (shift (fn [k] (k nil)))
                (catch clojure.lang.ExceptionInfo _
                  (reset! a true)
                  false))
              (throw (ex-info "test" {}))
              (catch clojure.lang.ExceptionInfo _))
            @a)))
    (is (reset
          (let [a (atom true)]
            (try
              (try
                (catch clojure.lang.ExceptionInfo _
                  (reset! a false)
                  false))
              (shift (fn [k] (k nil)))
              (throw (ex-info "test" {}))
              (catch clojure.lang.ExceptionInfo _))
            @a))))

  (testing "Shift in WITH-META"
    (is (= (meta (reset ^{:a (shift (fn [k] (k true)))} [1 2 3]))
           {:a true}))
    (is (= (meta (reset ^{:a true} [(shift (fn [k] (k 1))) 2 3]))
           {:a true}))
    (is (= (meta (reset ^{:a true} (fn [x] x)))
           {:a true}))
    (is (= (reset ^{:a (shift (fn [k] [(meta (k 1)) (meta (k 2))]))} [])
           [{:a 1} {:a 2}]))
    (let [[v1 v2] (reset ^{:a (shift (fn [k] [(k true) (k false)]))} [1 2 3])]
      (is (and
           (= (meta v1) {:a true})
           (= (meta v2) {:a false})))))

  (testing "Shift and Effects"
    (let [e (atom 0)]
      (letfn
       [(n3-with-effect []
          (swap! e inc)
          3)]

        (reset
         (+ 1 (n3-with-effect) (shift identity)))

        (is (= @e 1))

        (reset
         (+ (shift identity) (n3-with-effect)))

        (is (= @e 1))

        (reset
         (let [_ (n3-with-effect)
               b (shift identity)]
           b))

        (is (= @e 2))

        (reset
         (let [a (shift identity)]
           (+ a (n3-with-effect))))

        (is (= @e 2)))))

  (testing "Shift with Synchronization Primitives"
    (reset
     (let [m (atom nil)]
       (locking (shift (fn [k] (k m)))
         (is (swap! m not))))))

  (testing "Local Handler Declaration"
    (is (= (let [f (fn-shift [a] (shift (fn [k] (k [(+ a 1) (+ a 2)]))))]
             (reset
              (f 1)))
           [2 3]))

    (is (= (reset
            (let [f (fn-shift
                     [a]
                     (shift (fn [k]
                              [(k (+ a 1)) (k (+ a 2))])))]
              (+ 1 (f 1))))
           [3 4]))

    (is (= (reset
            (let [f (fn-shift
                     [a]
                     (let [g (fn-shift [x] (shift (fn [_] x)))]
                       (shift
                        (fn [k]
                          [(k (+ a 1)) (k (+ a 2))]))
                       (g 2)))]
              (+ 1 (f 1))))
           [2 2]))
    (is (= (reset
            (let [f (fn-shift [_] (shift (fn [_] 3)))]
              (if (> (shift (fn [k] (k 11))) 10)
                (do (f 2) 1)
                1)))
           3))
    (is (= (reset
            (let [f (fn-shift [_] (shift (fn [_] 3)))]
              (if (> (shift (fn [k] (k 11))) 10)
                (do (f 2) 1)
                1)))
           3))
    (is (= (reset
            (let [f (fn-shift
                     [_]
                     (shift (fn [_] 3))
                       ;; Unreachable
                     (recur 1))]
              (if (> (shift (fn [k] (k 11))) 10)
                (do (f 2) 1)
                1)))
           3)))

  (testing "Looping Handler"
    (is (=
         (reset
          (let [count-down
                (fn-shift
                 [x]
                 (if (> x 0)
                   (recur (dec x))
                   x))]
            (count-down 10)))
         0))
    (is (=
         (reset
          (let [count-down
                (fn-shift
                 [x]
                 (if (> x 0)
                   (recur (dec x))
                   x))]
            (count-down 10)))
         0))
    (is (=
         (reset
          (let [count-down
                (fn-shift
                 [x]
                 (if (> x 0)
                   (recur (shift (fn [k] (k (dec x)))))
                   x))]
            (count-down 10)))
         0))
    (is (=
         (reset
          (let [g (fn-shift
                   [x]
                   (if (> x 0)
                     (recur (dec x))
                     (shift (fn [_] 2))))]
            (g 10)))
         2)))

  (testing "Error handling"
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset b5))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset (do (let [a6 1] (shift identity)) a6)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(let [] b6))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset (fn*)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset (let* 1)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset (if)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval '(reset (loop*)))))))
