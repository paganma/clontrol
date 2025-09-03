(ns clontrol.analyzer.node-test
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.analyzer.node
    :as node
    :refer [every-child?
            node?
            read-children
            some-child?]]))

(def terminal-node
  {:op :terminal :children [] :env {} :form nil})

(def nonterminal-node
  {:op :nonterminal
   :a terminal-node
   :b terminal-node
   :c [terminal-node terminal-node]
   :children [:a :b :c]
   :env {}
   :form nil})

(deftest test-querying
  (testing "Children Count"
    (testing "Empty Node"
      (is (= (count (read-children terminal-node)) 0)))
    (testing "Non-empty Node"
      (is (= (count (read-children nonterminal-node)) 4)))))

(deftest test-predicates
  (testing "Node Format"
    (testing "Valid"
      (is (node? terminal-node))
      (is (node? {:op :test :children [] :env {} :form nil}))
      (is (node?
           {:op :test
            :c1 terminal-node
            :c2 terminal-node
            :children [:c1 :c2]
            :env {}
            :form nil})))

    (testing "Invalid"
      (is (not (node? {:op :test :children [] :env {}})))
      (is (not (node? {:op :test :children [] :form nil})))
      (is (not (node? {:op :test :form nil})))
      (is (not (node? {:children [] :form nil :env {}})))))

  (testing "Every Child"
    (is (every-child?
         (fn [node]
           (= (:op node) :terminal))
         {:op :test
          :c1 terminal-node
          :c2 terminal-node
          :children [:c1 :c2]
          :env {}
          :form nil})))

  (testing "Some Child"
    (is (some-child?
         (fn [node]
           (= (:op node) :terminal-1))
         {:op :test
          :c1 terminal-node
          :c2 terminal-node
          :c3 {:op :terminal-1
               :children []
               :env {}
               :form nil}
          :children [:c1 :c2 :c3]
          :env {}
          :form nil}))))
