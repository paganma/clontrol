(ns clontrol.operator.intrinsic.thunk-test
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.operator.intrinsic.thunk
    :as thunk
    :refer [make-thunk
            thunk
            thunk?]]))

(deftest thunk-test

  (testing "Creating Thunks"
    (is (thunk? (make-thunk (fn [] (+ 1 1)))))
    (is (thunk? (make-thunk (fn [] (make-thunk #(+ 1 1)))))))

  (testing "Running Thunks"
    (is (= (thunk/run (make-thunk (fn [] (+ 1 1)))) 2))
    (is (= (thunk/trampoline (make-thunk (fn [] (+ 1 1)))) 2))
    (is (= (thunk/trampoline (make-thunk (fn [] (make-thunk (fn [] (+ 1 1)))))) 2)))

  (testing "Thunk Macro"
    (is (thunk? (thunk 1)))
    (is (= (thunk/run (thunk (+ 1 1))) 2))
    (is (= (thunk/trampoline (thunk (thunk 1))) 1))))
