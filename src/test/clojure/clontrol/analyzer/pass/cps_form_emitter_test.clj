(ns clontrol.analyzer.pass.cps-form-emitter-test
  (:require
   [clojure.set]
   [clojure.string]
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.analyzer
    :as analyzer
    :refer [*make-local-environment*
            *scheduled-pass*]]
   [clontrol.analyzer.pass.cps-form-emitter
    :refer [run-cps-form-emitter]]
   [clontrol.operator
    :refer [shift]]))

(defn- equivalent-to?
  ([a-form b-form]
   (equivalent-to? #{} a-form b-form))
  ([bound-symbols a-form b-form]
   (letfn [(compare-nodes [x y state]
             (if (and (coll? x) (coll? y))
               (and (= (count x) (count y))
                    (every? identity (map #(compare-nodes %1 %2 state) x y)))
               (do (when (contains? bound-symbols y)
                     (swap! state assoc y x))
                   (or (= x y)
                       (= x (y @state))))))]
     (compare-nodes a-form b-form (atom {})))))

(defn- emit-cps
  [continuation-form body-form]
  (let [local-environment
        (merge
         (*make-local-environment*)
         {:passes-opts {:cps-form-emitter/continuation-form continuation-form
                        :cps-form-emitter/thunk-recur? false}
          :context :ctx/return})]
    (binding [*scheduled-pass* run-cps-form-emitter]
      (analyzer/analyze body-form local-environment))))

(defn- emit-test
  [continuation-form body-form]
  `(~'equivalent-to?
    (~'emit-cps
     (quote ~continuation-form)
     (quote ~body-form))
    (quote ~(emit-cps continuation-form body-form))))

(deftest form-conversion-test
  (testing "Conversion of IMPORT"
    (is (equivalent-to?
         (emit-cps `identity `(clojure.core/import* "class"))
         `(clojure.core/import* "class")))
    (is (equivalent-to?
         (emit-cps `k `(clojure.core/import* "class"))
         `(k (clojure.core/import* "class")))))

  (testing "Conversion of DEF"
    (is (equivalent-to?
         (emit-cps `identity '(def v 1))
         '(def v 1)))
    (is (equivalent-to?
         (emit-cps 'k '(def v 1))
         '(k (def v 1))))
    (is (equivalent-to?
         (emit-cps 'k '(def v))
         '(k (def v))))
    (is (equivalent-to?
         '#{k x x1}
         (emit-cps 'k `(def ~'v (shift (fn [~'k]))))
         '((fn* ([x]))
           (fn* ([x1] (k (def v x1)))))))
    (is (equivalent-to?
         '#{k x t}
         (emit-cps
          'k
          '(def t "doc" (clontrol.operator/shift clojure.core/identity)))
         '(clojure.core/identity (fn* ([x] (k (def t "doc" x))))))))

  (testing "Conversion of DEFN"
    (is (equivalent-to?
         (emit-cps `identity '(defn f [x]))
         '(def f (clojure.core/fn ([x])))))
    (is (equivalent-to?
         '#{k x f}
         (emit-cps
          'k
          '(clojure.core/defn
             ^{:test (clontrol.operator/shift clojure.core/identity)} f []))
         '(clojure.core/identity (fn* ([x] (k (def ^{:test x} f (fn* ([]))))))))))

  (testing "Conversion of MONITOR-ENTER"
    (equivalent-to?
     '#{a k r1}
     (emit-cps
      `identity
      `(let [~'a (atom)]
         (monitor-enter
          (shift (fn [~'k] (~'k ~'a))))))
     '(let* [a (atom)]
        ((fn* ([k] (k a)))
         (fn* ([r1] (monitor-enter r1)))))))

  (testing "Conversion of MONITOR-EXIT"
    (equivalent-to?
     '#{a k r1}
     (emit-cps
      `identity
      `(let [~'a (atom)]
         (monitor-exit (shift (fn [~'k] (~'k ~'a))))))
     '(let* [a (atom)]
        ((fn* ([k] (k a)))
         (fn* ([r1] (monitor-exit r1)))))))

  (testing "Conversion of SET!"
    (testing "With VAR Assignee"
      (is (equivalent-to?
           (emit-cps
            `identity
            '(set! *warn-on-reflection* true))
           '(set! *warn-on-reflection* true)))
      (is (equivalent-to?
           (emit-cps
            'k
            '(set! *warn-on-reflection* true))
           '(k (set! *warn-on-reflection* true))))))

  (testing "Emit DEFTYPE"
    (is (equivalent-to?
         '#{class-symbol class-import class-string}
         (emit-cps
          `identity
          '(deftype* Test package.Test []
             :implements []))
         '(deftype* Test package.Test []
            :implements []))))

  (testing "Error handling"
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (emit-cps 'k '(loop x x))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (emit-cps 'k '(fn 1))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (emit-cps 'k '(letfn* x))))
    (is (thrown?
         clojure.lang.Compiler$CompilerException
         (emit-cps 'k '(let* v1312))))))
