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
    :refer [invoke-shift
            invoke-unknown
            invoke-shift]]))

(deftest fn-cc-test
  (testing "Multiple Arities"
    (is (= (invoke-shift
            (fn-shift [x] x)
            identity 1)
           1))
    (is (= (invoke-shift
            (fn-shift [x y] x)
            identity 1 2)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z] x)
            identity 1 2 3)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a] x)
            identity 1 2 3 4)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b] x)
            identity 1 2 3 4 5)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c] x)
            identity 1 2 3 4 5 6)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d] x)
            identity 1 2 3 4 5 6 7)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e] x)
            identity 1 2 3 4 5 6 7 8)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f] x)
            identity 1 2 3 4 5 6 7 8 9)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g] x)
            identity 1 2 3 4 5 6 7 8 9 10)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h] x)
            identity 1 2 3 4 5 6 7 8 9 10 11)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h i] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h i l] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12 13)
           1))
    (is (= (invoke-shift
            (fn-shift [x] x)
            identity 1)
           1))
    (is (= (invoke-shift
            (fn-shift [x y] x)
            identity 1 2)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z] x)
            identity 1 2 3)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a] x)
            identity 1 2 3 4)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b] x)
            identity 1 2 3 4 5)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c] x)
            identity 1 2 3 4 5 6)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d] x)
            identity 1 2 3 4 5 6 7)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e] x)
            identity 1 2 3 4 5 6 7 8)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f] x)
            identity 1 2 3 4 5 6 7 8 9)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g] x)
            identity 1 2 3 4 5 6 7 8 9 10)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h] x)
            identity 1 2 3 4 5 6 7 8 9 10 11)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h i] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h i l] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12 13)
           1))
    (is (= (invoke-shift
            (fn-shift [x y z a b c d e f g h i l p1 p2 p3 p4 p5 p6] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
           1))
    (is (= (invoke-unknown
            (fn-shift [x] x)
            identity 1)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y] x)
            identity 1 2)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z] x)
            identity 1 2 3)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a] x)
            identity 1 2 3 4)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b] x)
            identity 1 2 3 4 5)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c] x)
            identity 1 2 3 4 5 6)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d] x)
            identity 1 2 3 4 5 6 7)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e] x)
            identity 1 2 3 4 5 6 7 8)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e f] x)
            identity 1 2 3 4 5 6 7 8 9)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e f g] x)
            identity 1 2 3 4 5 6 7 8 9 10)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e f g h] x)
            identity 1 2 3 4 5 6 7 8 9 10 11)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e f g h i] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12)
           1))
    (is (= (invoke-unknown
            (fn-shift [x y z a b c d e f g h i l] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12 13)
           1))
    (is (= (invoke-unknown
            (fn [x] x)
            identity 1)
           1))
    (is (= (invoke-unknown
            (fn [x y] x)
            identity 1 2)
           1))
    (is (= (invoke-unknown
            (fn [x y z] x)
            identity 1 2 3)
           1))
    (is (= (invoke-unknown
            (fn [x y z a] x)
            identity 1 2 3 4)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b] x)
            identity 1 2 3 4 5)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c] x)
            identity 1 2 3 4 5 6)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d] x)
            identity 1 2 3 4 5 6 7)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e] x)
            identity 1 2 3 4 5 6 7 8)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e f] x)
            identity 1 2 3 4 5 6 7 8 9)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e f g] x)
            identity 1 2 3 4 5 6 7 8 9 10)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e f g h] x)
            identity 1 2 3 4 5 6 7 8 9 10 11)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e f g h i] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12)
           1))
    (is (= (invoke-unknown
            (fn [x y z a b c d e f g h i l] x)
            identity 1 2 3 4 5 6 7 8 9 10 11 12 13)
           1)))
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
          identity true))))
(testing "Error handling"
    (is (thrown?
         clojure.lang.ExceptionInfo
         (eval `((fn-shift [])))))
    (is (thrown?
         clojure.lang.ExceptionInfo
         (eval `((fn-shift [p1#]) 1))))
    (is (thrown?
         clojure.lang.ExceptionInfo
         (eval `((fn-shift [p1# p2#]) 1 2))))
    (is (thrown?
         clojure.lang.ExceptionInfo
         (eval `((fn-shift [p1# p2# p3#]) 1 2 3))))))

(deftest defn-cc-test
  (testing "Well formed functions"
    (is (macroexpand `(defn-shift f# [x#] x#)))
    (is (macroexpand `(defn-shift f# [~'&form ~'&env x#] x#))))
  (testing "Attaching Metadata"
    (is (macroexpand `(defn-shift f# [x#] {:meta true} x#)))
    (is (macroexpand `(defn-shift f# [x#] {:meta false}))))
  (testing "Attaching tags"
    (is (macroexpand `(defn-shift f# ^Integer [^Integer x#] x#)))))
