(ns clontrol.operator.intrinsic.thunk-test
  (:import
   [clontrol.operator.intrinsic.thunk Thunk])
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.operator.intrinsic.thunk
    :as thunk
    :refer [thunk
            thunk?]]))

(deftest thunk-test

  (testing "Creating Thunks"
    (is (thunk? (Thunk. (fn [] (+ 1 1)))))
    (is (thunk? (Thunk. (fn [] (Thunk. #(+ 1 1)))))))

  (testing "Running Thunks"
    (is (= (thunk/trampoline (Thunk. (fn [] (+ 1 1)))) 2))
    (is (= (thunk/trampoline (Thunk. (fn [] (Thunk. (fn [] (+ 1 1)))))) 2)))

  (testing "Thunk Macro"
    (is (thunk? (thunk 1)))
    (is (= (thunk/trampoline (thunk (thunk 1))) 1))))
