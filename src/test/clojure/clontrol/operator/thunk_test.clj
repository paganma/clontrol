(ns clontrol.operator.thunk-test
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.operator.thunk
    :as thunk
    :refer [thunk
            thunk?]]))

(deftest thunk-test

  (testing "Creating Thunks"
    (is (thunk? (thunk (+ 1 1))))
    (is (thunk? (thunk (thunk (+ 1 1))))))

  (testing "Running Thunks"
    (is (= (thunk/trampoline (thunk (+ 1 1))) 2))
    (is (= (thunk/trampoline (thunk (thunk (+ 1 1)))) 2))))
