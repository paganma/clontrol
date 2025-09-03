(ns clontrol.analyzer-test
  (:require
   [clojure.test
    :refer [deftest
            is]]
   [clontrol.analyzer
    :refer [*make-global-environment*
            *make-local-environment*
            *scheduled-pass*
            analyze
            global-environment?
            local-environment?]]
   [clontrol.analyzer.node
    :refer [node?]]))

(deftest global-environment-test
  (is (global-environment? (*make-global-environment*)))
  (is (not (global-environment? nil)))
  (is (not (global-environment? (swap! (*make-global-environment*) dissoc :namespaces)))))

(deftest local-environment-test
  (is (local-environment? (*make-local-environment*)))
  (is (not (local-environment? nil)))
  (is (not (local-environment? (dissoc (*make-local-environment*) :ns)))))

(deftest analyze-test
  (is (node? (analyze '(+ 1 2 3))))
  (binding [*scheduled-pass* (fn [node] nil)]
    (is (= (analyze '(+ 1 2 3)) nil))))
