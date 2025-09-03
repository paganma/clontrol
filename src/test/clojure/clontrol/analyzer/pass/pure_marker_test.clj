(ns clontrol.analyzer.pass.pure-marker-test
  (:require
   [clojure.test
    :refer [deftest
            testing
            is]]
   [clontrol.analyzer
    :as analyzer]
   [clontrol.analyzer.pass.pure-marker
    :refer [run-pure-marker]]))

(defn marked-pure?
  [form]
  (:pure? (run-pure-marker (analyzer/analyze form))))

(deftest pure-marker-test

  (testing "Pure Constants"
    (is (marked-pure? 1))
    (is (marked-pure? "test"))
    (is (marked-pure? 3.14))
    (is (marked-pure? :test)))

  (testing "Pure Expressions"
    (is (marked-pure? `(fn [] 1)))
    (is (marked-pure? `(fn [] (fn [] 1))))
    (is (marked-pure? `[1 2 3]))
    (is (marked-pure? `{:test 1})))

  (testing "Pure Loops"
    (is (marked-pure? `(loop [a# 10] a#)))
    (is (marked-pure? `(loop [a# 10] (fn [] 1))))
    (is (marked-pure? `(loop [a# 10] (recur a#))))))
