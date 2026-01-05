(ns clontrol.core-test
  (:require
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.core
    :refer [fn-shift
            reset
            shift]]))

(deftest core-test
  (testing "Composability"
    (is (let [a
              (fn-shift
               [x]
               (shift (fn [k] [(k 1) (k 2)])))]
          (= (reset (a 1)) [1 2]))))

  (testing "Examples"
    (testing "CC Generator"
      (let [yield
            (fn-shift
              [x]
              (shift (fn [k] [k x])))]
        (letfn
            [(step [[k x]] (k x))
             (fetch [[_ x]] x)]
          (let [generator
                (reset
                  (loop [a 0]
                    (yield a)
                    (recur (+ a 1))))]
            (is (= (fetch (step generator)) 1))
            (is (= (fetch (step (step generator))) 2))))))

    (testing "Stateful Generator with Handlers"
      (let [yield (fn-shift [x] (shift (fn [k] [k x])))
            fetch (fn [t] (second @t))
            step (fn [t] (reset! t ((first @t) (second @t))))
            generator
            (atom
             (reset
               (loop [x 0]
                 (yield (+ x 1))
                 (recur (inc x)))))]
        (dotimes [_ 10000]
          (step generator))
        (is (= (fetch generator) 10001)))))

  (testing "Local Handler Declaration"
    (is (= (let [f (fn-shift [a] (shift (fn [k] (k [(+ a 1) (+ a 2)]))))]
             (reset
               (f 1)))
           [2 3]))

    (is (= (reset
             (let [f (fn-shift
                       [a]
                       (shift (fn [k]
                                [(k (+ a 1)) (k (+ a 2))])))]
               (+ 1 (f 1))))
           [3 4]))

    (is (= (reset
             (let [f (fn-shift
                       [a]
                       (let [g (fn-shift [x] (shift (fn [_] x)))]
                         (shift
                             (fn [k]
                               [(k (+ a 1)) (k (+ a 2))]))
                         (g 2)))]
               (+ 1 (f 1))))
           [2 2]))
    (is (= (reset
             (let [f (fn-shift [_] (shift (fn [_] 3)))]
               (if (> (shift (fn [k] (k 11))) 10)
                 (do (f 2) 1)
                 1)))
           3))
    (is (= (reset
             (let [f (fn-shift [_] (shift (fn [_] 3)))]
               (if (> (shift (fn [k] (k 11))) 10)
                 (do (f 2) 1)
                 1)))
           3))
    (is (= (reset
             (let [f (fn-shift
                       [_]
                       (shift (fn [_] 3))
                       ;; Unreachable
                       (recur 1))]
               (if (> (shift (fn [k] (k 11))) 10)
                 (do (f 2) 1)
                 1)))
           3)))

  (testing "Looping Handler"
    (is (= (reset
             (let [count-down
                   (fn-shift
                     [x]
                     (if (> x 0)
                       (recur (dec x))
                       x))]
               (count-down 10)))
           0))
    (is (= (reset
             (let [count-down
                   (fn-shift
                     [x]
                     (if (> x 0)
                       (recur (dec x))
                       x))]
               (count-down 10)))
           0))
    (is (= (reset
             (let [count-down
                   (fn-shift
                     [x]
                     (if (> x 0)
                       (recur (shift (fn [k] (k (dec x)))))
                       x))]
               (count-down 10)))
           0))
    (is (= (reset
             (let [g (fn-shift
                       [x]
                       (if (> x 0)
                         (recur (dec x))
                         (shift (fn [_] 2))))]
               (g 10)))
           2))))
