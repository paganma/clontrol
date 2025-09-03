(ns clontrol.analyzer.exception-test
  (:require
   [clojure.test
    :refer [deftest
            is]]
   [clontrol.analyzer.exception
    :refer [make-compiler-exception compiler-exception?]]))

(deftest test-compiler-exception
  (is (compiler-exception? (make-compiler-exception :test "file" 1 1 nil))))
