(ns clontrol.function-test
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.function
    :refer [defn-shift
            fn-shift]]
   [clontrol.function.shifter
    :refer [invoke-shift]]))

(deftest fn-cc-test
  (testing "Destructuring Arguments"
    (is (= (invoke-shift
            (fn-shift [{x :x}] x)
            identity {:x 1})
           1))
    (is (= (invoke-shift
            (fn-shift [[x & _]] x)
            identity [1 2 3])
           1))
    (is (= (invoke-shift
            (fn-shift [[_ & xs]] xs)
            identity [1 2 3])
           [2 3]))
    (is (= (invoke-shift
            (fn-shift [& {:keys [x]}] x)
            identity :x 1)
           1))
    (is (= (invoke-shift
            (fn-shift [& {:keys [x] :or {x 10}}] x)
            identity)
           10))
    (is (= (invoke-shift
            (fn-shift [& {:keys [x] :or {x 10}}] x)
            identity :x 1)
           1)))
  (testing "Malformed Arguments"
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift 1))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift)))))
  (testing "Malformed Signature"
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift (1)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift (x 1)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift ({} 1)))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift {} {} []))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift name# {} {} []))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift ([] 1) {}))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift ([] 1) ({})))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift ([] 1) ({})))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (eval `(fn-shift name# 1)))))
  (testing "Pre-Postconditions"
    (is (= (invoke-shift
            (fn-shift [] {:post [(> % 0)]} (+ 1 1) (+ 2 2))
            identity)
           4))
    (is (= (invoke-shift
            (fn-shift [x] {:pre [true] :post [%]} x)
            identity true)
           true))
    (is (= (invoke-shift
            (fn-shift [x] {:pre [x] :post [%]} x)
            identity true)
           true))
    (is (thrown?
         java.lang.AssertionError
         (invoke-shift
          (fn-shift [x] {:pre [false] :post [false]} x)
          identity true)))))

(deftest defn-cc-test
  (testing "Well formed functions"
    (is (macroexpand `(defn-shift f# [x#] x#)))
    (is (macroexpand `(defn-shift f# [~'&form ~'&env x#] x#))))
  (testing "Attaching Metadata"
    (is (macroexpand `(defn-shift f# [x#] {:meta true} x#)))
    (is (macroexpand `(defn-shift f# [x#] {:meta false}))))
  (testing "Attaching tags"
    (is (macroexpand `(defn-shift f# ^Integer [^Integer x#] x#)))))
