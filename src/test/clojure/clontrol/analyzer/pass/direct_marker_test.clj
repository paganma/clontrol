(ns clontrol.analyzer.pass.direct-marker-test
  (:require
   [clojure.test
    :refer [deftest
            testing
            is]]
   [clontrol.analyzer
    :as analyzer]
   [clontrol.analyzer.pass.direct-marker
    :refer [run-direct-marker]]
   [clontrol.operator
    :as operator
    :refer [shift]]))

(defn marked-direct?
  ([form]
   (marked-direct? form identity))
  ([form read-child]
   (:direct? (read-child (run-direct-marker (analyzer/analyze form))))))

(deftest direct-marker-test

  (testing "Direct Constants"
    (is (marked-direct? 1))
    (is (marked-direct? "test"))
    (is (marked-direct? 3.14))
    (is (marked-direct? :test))
    (is (marked-direct? {:test 1})))

  (testing "Direct Expressions"
    (is (marked-direct? '(+ 1 2 3)))
    (is (marked-direct? '(+ (+ 1 2 3) (+ 1 2 3))))
    (is (marked-direct? `(fn [] 1)))
    (is (marked-direct? `(fn [] (shift identity))))
    (is (marked-direct? `(quote 1))))

  (testing "Direct Loops"
    (is (marked-direct? '(loop [a 10] a))))

  (testing "Non-Direct Expressions"
    (is (not (marked-direct? `(shift (fn [k#] 1)))))
    (is (not (marked-direct? `(+ 1 (shift (fn [k#] 1))))))
    (is (not (marked-direct? `(str "Hello" (shift (fn [k#] (k# "World")))))))
    (is (not (marked-direct? `(loop [] (+ 1 (shift (fn [k#] k#)))))))
    (is (not (marked-direct?
              `(loop [] (shift (fn [k#] k#)) (recur))
              (comp :ret :body))))))
