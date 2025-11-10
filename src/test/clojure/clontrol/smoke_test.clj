(ns clontrol.smoke-test
  (:require
   [clojure.set]
   [clojure.string]
   [clojure.test
    :refer [deftest
            is
            testing]]
   [clontrol.core
    :refer [reset
            shift
            fn-shift]]))

(set! *warn-on-reflection* false)

(defmacro smoke
  [value]
  `(shift (fn [k#] (k# ~value))))

;;;; * Algorithmic tests

(deftest test-algorithms
  (testing "DFS"
    (reset
     (let
      [dfs
       (fn-shift
        dfs [graph start]
        (loop [stack [start]
               visited (smoke #{})]
          (if ((smoke empty?) stack)
            visited
            (let [current (peek stack)
                  neighbors (get graph current [])]
              (if ((smoke contains?) visited current)
                (recur (smoke (pop stack)) visited)
                (recur (into (smoke (pop stack)) neighbors)
                       (conj visited current)))))))]
       (let [graph {:a [:b :c]
                    :b [:d]
                    :c [:e]
                    :d []
                    :e []}]
         (is (= (dfs graph :a) #{:a :b :c :d :e}))
         (is (= (dfs (smoke graph) :d) #{:d}))
         (is (= (dfs graph :e) #{:e}))
         (is ((smoke =) (dfs graph :b) #{:b :d})))
       (let [graph {1 [2 3]
                    2 [4]
                    3 [(smoke 5)]
                    4 []
                    5 []}]
         (is (= (dfs (smoke graph) 2) #{2 4}))
         (is (= (dfs graph 3) #{3 5}))
         (is (= (dfs graph 4) #{4}))
         (is (= (dfs graph 5) #{5}))
         (is (= (dfs graph 1) #{1 2 3 4 5}))))))

  (testing "GCD"
    (reset
     (let [gcd
           (fn-shift
            gcd
            ([a] (Math/abs (smoke a)))
            ([a b]
             (let [abs-a (Math/abs a)
                   abs-b (Math/abs (smoke b))]
               (if ((smoke zero?) abs-b)
                 abs-a
                 (recur (smoke abs-b) (mod abs-a (smoke abs-b))))))
            ([a b & more]
             (let [g ((smoke gcd) a b)]
               (if (empty? (smoke more))
                 g
                 (apply gcd (smoke g) more)))))]
       (is (= (gcd 48 18) 6))
       (is (= (gcd 0 5) 5))
       (is (= (gcd 5 0) 5))
       (is (= (gcd 0 0) 0))
       (is (= (gcd -15 25) 5))
       (is (= (gcd 99 100) 1))
       (is (= (gcd 17) 17)))))

  (testing "GCD Shift"
    (reset
     (let [^:shift gcd
           (fn-shift
            gcd
            ([a] (Math/abs (smoke a)))
            ([a b]
             (let [abs-a (Math/abs a)
                   abs-b (Math/abs (smoke b))]
               (if ((smoke zero?) abs-b)
                 abs-a
                 (recur (smoke abs-b) (mod abs-a (smoke abs-b))))))
            ([a b & more]
             (let [g ((smoke gcd) a b)]
               (if (empty? (smoke more))
                 g
                 (apply gcd (smoke g) more)))))]
       (is (= (gcd 48 18) 6))
       (is (= (gcd 0 5) 5))
       (is (= (gcd 5 0) 5))
       (is (= (gcd 0 0) 0))
       (is (= (gcd -15 25) 5))
       (is (= (gcd 99 100) 1))
       (is (= (gcd 17) 17)))))

  (testing "Insertion Sort"
    (reset
     (let [insertion-sort
           (fn-shift
            [coll]
            (let [insert
                  (fn-shift
                   [x sorted]
                   (loop [head [] tail sorted]
                     (cond
                       (empty? tail) (smoke (conj head x))
                       (<= x (first (smoke tail))) (concat head [x] tail)
                       :else
                       (recur
                        (smoke (conj head (first tail)))
                        (smoke (rest tail))))))]
              (loop [sorted '()
                     unsorted (seq coll)]
                (if (empty? (smoke unsorted))
                  sorted
                  (recur
                   (insert ((smoke first) unsorted) sorted)
                   (smoke (rest unsorted)))))))]
       (is (= (insertion-sort []) []))
       (is (= (insertion-sort [5]) [5]))
       (is (= (insertion-sort [9 2 6]) [2 6 9]))
       (is (= (insertion-sort [3 1 4 1 5 9 2 6]) [1 1 2 3 4 5 6 9]))
       (is (= (insertion-sort (range 10 0 -1)) (range 1 11)))
       (is (= (insertion-sort (list)) (list)))
       (is (= (insertion-sort (cons 5 nil)) [5])))))

  (testing "Quicksort"
    (reset
     (let [quicksort
           (fn-shift
            quicksort
            [coll]
            (if (<= (count (smoke coll)) 1)
              coll
              (let [pivot (first (smoke coll))
                    smaller (smoke (filter #(< % pivot) (rest coll)))
                    larger (smoke (filter #(>= % pivot) (rest coll)))]
                (concat
                 (quicksort smaller)
                 (smoke [pivot])
                 (quicksort larger)))))]
       (is (= (quicksort []) []))
       (is (= (quicksort [5]) [5]))
       (is (= (quicksort [9 2 6]) [2 6 9]))
       (is (= (quicksort [3 1 4 1 5 9 2 6]) [1 1 2 3 4 5 6 9]))
       (is (= (quicksort (range 10 0 -1)) (range 1 11)))
       (is (= (quicksort (list)) (list)))
       (is (= (quicksort (cons 5 nil)) [5])))))

  (testing "Merge sort"
    (reset
     (let [merge-sort
           (fn-shift
            merge-sort
            [coll]
            (if (<= (count coll) 1)
              coll
              (let [mid (quot (count coll) 2)
                    left (take (smoke mid) coll)
                    right (drop mid coll)
                    merge (fn merge [left right]
                            (lazy-seq
                             (cond
                               (empty? left) right
                               (empty? right) left
                               :else
                               (if (<= (first left) (first right))
                                 (cons (first left)
                                       (merge (rest left) right))
                                 (cons (first right)
                                       (merge left (rest right)))))))]
                (merge (merge-sort left) (merge-sort (smoke right))))))]
       (is (= (merge-sort []) []))
       (is (= (merge-sort [5]) [5]))
       (is (= (merge-sort [9 2 6]) [2 6 9]))
       (is (= (merge-sort [3 1 4 1 5 9 2 6]) [1 1 2 3 4 5 6 9]))
       (is (= (merge-sort (range 10 0 -1)) (range 1 11)))
       (is (= (merge-sort (list)) (list)))
       (is (= (merge-sort (cons 5 nil)) [5]))))))

;;;; * Reset invariance

(defn remove-smoke
  ([form]
   (trampoline remove-smoke identity form))
  ([return form]
   (letfn [(traverse-children
             ([return child-forms]
              (traverse-children return child-forms []))
             ([return child-forms result-forms]
              (if (seq child-forms)
                (let [[child-form & child-forms'] child-forms]
                  (remove-smoke
                   (fn [child-form']
                     (let [result-forms' (conj result-forms child-form')]
                       (traverse-children return child-forms' result-forms')))
                   child-form))
                (return result-forms))))]
     (if (and (seq? form) (= (first form) 'smoke))
       #(remove-smoke return (second form))
       (cond
         (list? form)
         #(traverse-children
           (fn [child-forms']
             (return (apply list child-forms')))
           form)
         (seq? form)
         #(traverse-children
           (fn [child-forms']
             (return (lazy-seq child-forms')))
           form)
         (vector? form)
         #(traverse-children return form)
         (map? form)
         #(traverse-children
           (fn [child-forms']
             (return (into {} child-forms')))
           form)
         (set? form)
         #(traverse-children
           (fn [child-forms']
             (return (into #{} child-forms')))
           form)
         :else
         (return form))))))

(defmacro reset-invariant?
  [& body]
  (let [body-form (list* 'do body)]
    `(= (do ~(remove-smoke body-form))
        (reset ~body-form))))

#_{:clj-kondo/ignore
   [:redundant-let
    :redundant-do
    :unused-binding
    :unused-value]}
(deftest test-reset-invariance

  (is (reset-invariant?
       1))

  (is (reset-invariant?
       3.14))

  (is (reset-invariant?
       "string"))

  (is (reset-invariant?
       'symbol))

  (is (reset-invariant?
       '()))

  (is (reset-invariant?
       (quote (if a b c))))

  (is (reset-invariant?
       ((smoke +) 1 2 3 4)))

  (is (reset-invariant?
       (+ 1 2 (smoke 3) 4)))

  (is (reset-invariant?
       (reduce (smoke +) '(1 2 3 4))))

  (is (reset-invariant?
       (do
         (let [a -1
               b -2
               c 0]
           (let [a 1] (let [b 2] (let [c 3] (smoke nil))))
           [a b c]))))

  (is (reset-invariant?
       (let [a -1
             b -2
             c 0]
         (let [a (smoke 1)]
           (let [b (smoke 2)]
             (let [c (smoke 3)]
               (smoke nil))))
         [a b (smoke c)])))

  (is (reset-invariant?
       (let [a 1
             b 2
             c (smoke 3)]
         (if (let [a true b (smoke false)] (and a b))
           nil
           (let [a (smoke 4)
                 b (let [a (smoke 1)]
                     (let [b (smoke 2)]
                       (let [c (smoke 3)]
                         (smoke nil))))
                 c (smoke 6)]
             [a b (smoke c)])))))

  (is (reset-invariant?
       (if (let [a true b (smoke false)] (and a b))
         nil
         (let [c 1]
           (+ 1 2 3
              (let [a (smoke 1)]
                (let [b (smoke 2)]
                  (let [c (smoke 3)]
                    (smoke c)))))))))

  (is (reset-invariant?
       (if (let [a true b (smoke false)] (and a b))
         nil
         (let [c 1]
           (+ 1
              (do (let [a 2] a))
              3
              (let [a (smoke 1)]
                (let [b (smoke 2)]
                  (let [c (smoke 3)]
                    (smoke c)))))))))

  (is (reset-invariant?
       (do
         (let [a -1
               b -2
               c 0]
           (+ 1 (let [a 1]
                  (let [b 2]
                    (let [c 3]
                      (smoke 2)))))
           [a b c]))))

  (is (reset-invariant?
       (-> {:a (smoke 1), (smoke :b) 2}
           (assoc (smoke :c) 3) (update :a inc))))

  (is (reset-invariant?
       (let [[x y & z] [(smoke 1) 2 (smoke 3) 4 5]]
         [x (smoke y) z])))

  (is (reset-invariant?
       (let [a {:x (smoke 1), (smoke :y) [2 3 4]}
             b (update (smoke a) :y conj 5)
             c (assoc b :z {:a 10, :b 20})
             d (update-in (smoke c) [:z :b] * (smoke 2))
             e (merge (smoke d) {(smoke :w) 100})]
         (select-keys (smoke e) [:x :y :z :w]))))

  (is (reset-invariant?
       (let [a #{(smoke 1) (smoke 2) (smoke 3) 4}
             b #{3 (smoke 4) 5 6}
             c ((smoke clojure.set/union) a b)
             d (clojure.set/difference (smoke a) b)
             e (clojure.set/intersection a (smoke b))]
         [c (smoke d) e])))

  (is (reset-invariant?
       (let [a [1 (smoke 2) 3 (smoke 4) 5]
             b (mapv (smoke #(* % 2)) a)
             c ((smoke filterv) even? b)
             d ((smoke reduce) (smoke +) c)
             e (conj (smoke c) d)]
         (smoke e))))

  (is (reset-invariant?
       (let [a {(smoke :a) 1, :b 2, :c 3}
             b (update a :a (smoke inc))
             c (assoc b (smoke :d) 4)
             d (dissoc c :b)
             e (into {} (map (smoke (fn [[k v]] [k (* v 2)])) d))]
         e)))

  (is (reset-invariant?
       (let [a 10
             b 20
             c (if (> a b)
                 :a
                 :b)
             d (cond
                 (< a b) :less
                 (= a b) :equal
                 :else :greater)
             e (case c
                 :a "A"
                 :b "B"
                 "C")]
         [c d e])))

  (is (reset-invariant?
       (let [x (smoke 42)
             y (smoke 100)
             z (+ x y)
             w (* z 2)
             v (+ w 50)]
         (smoke (/ v 10)))))

  (is (reset-invariant?
       (let [a (smoke 10)
             b (smoke 20)
             c (+ a b)
             d (* c 3)
             e (- d 15)
             f (/ e 5)]
         (smoke (+ f 100)))))

  (is (reset-invariant?
       (let [x (smoke 5)
             y (smoke 10)
             z (* x y)
             w (+ z 100)
             v (/ w 2)
             u (- v 25)]
         (smoke (* u 3)))))

  (is (reset-invariant?
       (let [a (smoke 1)
             b (smoke 2)
             c (smoke 3)
             d (smoke 4)
             e (+ a b)
             f (+ c d)
             g (* e f)
             h (- g 10)]
         (smoke (/ h 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (smoke 25)
             z (/ x y)
             w (* z 10)
             v (+ w 100)
             u (- v 50)]
         (smoke (/ u 5)))))

  (is (reset-invariant?
       (let [a (smoke 7)
             b (smoke 8)
             c (smoke 9)
             d (+ a b)
             e (+ c d)
             f (* e 2)
             g (- f 10)]
         (smoke (/ g 3)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (smoke 200)
             z (+ x y)
             w (/ z 2)
             v (* w 3)
             u (- v 150)]
         (smoke (+ u 50)))))

  (is (reset-invariant?
       (let [a (smoke 12)
             b (smoke 13)
             c (smoke 14)
             d (+ a b)
             e (+ c d)
             f (* e 2)
             g (- f 10)
             h (/ g 5)]
         (smoke (+ h 100)))))

  (is (reset-invariant?
       (let [x (smoke 15)
             y (smoke 30)
             z (* x y)
             w (- z 100)
             v (/ w 2)
             u (+ v 50)]
         (smoke (* u 3)))))

  (is (reset-invariant?
       (let [a (smoke 3)
             b (smoke 4)
             c (smoke 5)
             d (* a b)
             e (* c d)
             f (+ e 10)
             g (- f 5)
             h (/ g 2)]
         (smoke (+ h 100)))))

  (is (reset-invariant?
       (let [x (smoke 42)
             y (smoke 100)
             z (+ x y)
             w (if (> z 150) (* z 2) (/ z 2))
             v (+ w 50)]
         (smoke (/ v 10)))))

  (is (reset-invariant?
       (let [a (smoke 10)
             b (smoke 20)
             c (+ a b)
             d (if (< c 50) (* c 3) (/ c 3))
             e (- d 15)
             f (/ e 5)]
         (smoke (+ f 100)))))

  (is (reset-invariant?
       (let [x (smoke 5)
             y (smoke 10)
             z (* x y)
             w (if (even? z) (+ z 100) (- z 100))
             v (/ w 2)
             u (- v 25)]
         (smoke (* u 3)))))

  (is (reset-invariant?
       (let [a (smoke 1)
             b (smoke 2)
             c (smoke 3)
             d (smoke 4)
             e (+ a b)
             f (+ c d)
             g (if (> e f) (* e f) (/ e f))
             h (- g 10)]
         (smoke (/ h 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (smoke 25)
             z (/ x y)
             w (if (< z 3) (* z 10) (/ z 10))
             v (+ w 100)
             u (- v 50)]
         (smoke (/ u 5)))))

  (is (reset-invariant?
       (let [a (smoke 7)
             b (smoke 8)
             c (smoke 9)
             d (+ a b)
             e (+ c d)
             f (if (odd? e) (* e 2) (/ e 2))
             g (- f 10)]
         (smoke (/ g 3)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (smoke 200)
             z (+ x y)
             w (if (> z 300) (/ z 2) (* z 2))
             v (* w 3)
             u (- v 150)]
         (smoke (+ u 50)))))

  (is (reset-invariant?
       (let [a (smoke 12)
             b (smoke 13)
             c (smoke 14)
             d (+ a b)
             e (+ c d)
             f (if (< e 50) (* e 2) (/ e 2))
             g (- f 10)
             h (/ g 5)]
         (smoke (+ h 100)))))

  (is (reset-invariant?
       (let [x (smoke 15)
             y (smoke 30)
             z (* x y)
             w (if (even? z) (- z 100) (+ z 100))
             v (/ w 2)
             u (+ v 50)]
         (smoke (* u 3)))))

  (is (reset-invariant?
       (let [a (smoke 3)
             b (smoke 4)
             c (smoke 5)
             d (* a b)
             e (* c d)
             f (if (> e 50) (+ e 10) (- e 10))
             g (- f 5)
             h (/ g 2)]
         (smoke (+ h 100)))))

  (is (reset-invariant?
       (let [x (smoke 42)
             y (smoke 100)
             z (let [a (+ x y)
                     b (* a 2)]
                 (smoke (take 3 (range b))))]
         (smoke (reduce + z)))))

  (is (reset-invariant?
       (let [m (smoke {:a 1, :b 2, :c 3})
             n (let [updated (assoc m :d 4)
                     filtered (dissoc updated :b)]
                 (smoke (merge filtered {:e 5})))]
         (smoke (reduce + (vals n))))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          doubled (map #(* % 2) evens)]
                      (smoke (reduce + doubled)))]
         (smoke (* result 2)))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (let [a (range x)
                     b (map inc a)
                     c (filter odd? b)]
                 (smoke (reduce + c)))]
         (smoke (* y 3)))))

  (is (reset-invariant?
       (let [m (smoke {:x 10, :y 20, :z 30})
             result (let [keys (keys m)
                          vs (map #(* % 2) (vals m))
                          updated (zipmap keys vs)]
                      (smoke (reduce + (vals updated))))]
         (smoke (+ result 100)))))

  (is (reset-invariant?
       (let [lst (smoke [5 10 15 20 25 30])
             result (let [first-half (take 3 lst)
                          second-half (drop 3 lst)
                          combined (concat first-half (map #(* % 2) second-half))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 5)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 3)) a)
                     c (map #(* % 2) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 100)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat evens (map #(* % 3) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (range 0 x 10)
                     b (map #(* % 2) a)
                     c (filter #(> % 50) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [x (smoke 42)
             y (let [a (range x)
                     b (map #(* % 2) a)
                     c (filter even? b)]
                 (smoke (reduce + c)))]
         (smoke (* y 3)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat evens (map #(* % 2) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 5)) a)
                     c (map #(* % 3) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [m1 (smoke {:x 10, :y 20})
             m2 (smoke {:z 30, :w 40})
             result (let [merged (merge m1 m2)
                          updated (assoc merged :total (reduce + (vals merged)))]
                      (smoke (dissoc updated :w)))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [lst (smoke [5 10 15 20 25 30])
             result (let [first-half (take 3 lst)
                          second-half (drop 3 lst)
                          combined (concat first-half (map #(* % 2) second-half))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 5)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (range 0 x 10)
                     b (map #(* % 2) a)
                     c (filter #(> % 50) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat evens (map #(* % 3) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 5)) a)
                     c (map #(* % 3) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [x (smoke 42)
             y (let [a (range x)
                     b (map #(* % 2) a)
                     c (filter even? b)]
                 (smoke (reduce + c)))]
         (smoke (* y 3)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat evens (map #(* % 2) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 5)) a)
                     c (map #(* % 3) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [m1 (smoke {:x 10, :y 20})
             m2 (smoke {:z 30, :w 40})
             result (let [merged (merge m1 m2)
                          updated (assoc merged :total (reduce + (vals merged)))]
                      (smoke (dissoc updated :w)))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [lst (smoke [5 10 15 20 25 30])
             result (let [first-half (take 3 lst)
                          second-half (drop 3 lst)
                          combined (concat first-half (map #(* % 2) second-half))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 5)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (range 0 x 10)
                     b (map #(* % 2) a)
                     c (filter #(> % 50) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat evens (map #(* % 3) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 5)) a)
                     c (map #(* % 3) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (range 0 x 7)
                     b (map #(* % %) a)
                     c (filter #(even? %) b)]
                 (smoke (reduce + c)))]
         (smoke (/ y 10)))))

  (is (reset-invariant?
       (let [lst (smoke [1 3 5 7 9 11 13 15])
             result (let [odds (filter odd? lst)
                          squares (map #(* % %) odds)
                          sum (reduce + squares)]
                      (smoke (take 3 (range sum))))]
         (smoke (reduce + result)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 4)) a)
                     c (map #(* % 3) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 100)))))

  (is (reset-invariant?
       (let [m1 (smoke {:x 10, :y 20})
             m2 (smoke {:z 30, :w 40})
             result (let [merged (merge m1 m2)
                          updated (assoc merged :sum (reduce + (vals merged)))]
                      (smoke (select-keys updated [:x :z :sum])))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [lst (smoke [2 4 6 8 10 12 14 16])
             result (let [evens (filter even? lst)
                          doubled (map #(* % 2) evens)
                          sum (reduce + doubled)]
                      (smoke (take-last 3 (range sum))))]
         (smoke (reduce + result)))))

  (is (reset-invariant?
       (let [x (smoke 200)
             y (let [a (range 0 x 10)
                     b (map #(* % 3) a)
                     c (filter #(> % 100) b)]
                 (smoke (reduce + c)))]
         (smoke (- y 500)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (filter even? lst)
                          odds (filter odd? lst)
                          combined (concat
                                    (map #(* % 2) evens)
                                    (map #(+ % 1) odds))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 60)
             y (let [a (range 1 x)
                     b (filter #(zero? (mod % 6)) a)
                     c (map #(* % 5) b)]
                 (smoke (reduce + c)))]
         (smoke (+ y 1000)))))

  (is (reset-invariant?
       (let [m1 {:a 1, :b 2}
             m2 {:c 3, :d 4}
             result (let [merged (merge m1 m2)
                          updated (assoc merged :e 5)
                          keys (keys updated)
                          vals (map inc (vals updated))]
                      (zipmap keys vals))]
         (reduce + (vals result)))))

  (is (reset-invariant?
       (let [m (smoke {:a 3, :b 6, :c 9})
             result (let [keys (keys m)
                          vals (map #(* % 2) (vals m))
                          updated
                          (assoc (zipmap keys vals) :total (reduce + vals))]
                      (smoke (dissoc updated :b)))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [m (smoke {:a 3, :b 6, :c 9})
             result (let [updated (-> m (assoc :d 12))
                          keys (keys updated)
                          vals (map #(* % 2) (vals updated))]
                      (smoke (zipmap keys vals)))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [lst (smoke [1 3 5 7 9 11 13 15])
             result (let [squares (->> lst
                                       (map #(* % %))
                                       (filter even?))
                          sum (reduce + squares)]
                      (smoke (take 3 (range sum))))]
         (smoke (reduce + result)))))

  (is (reset-invariant?
       (let [m1 (smoke {:x 10, :y 20})
             m2 (smoke {:z 30, :w 40})
             result (let [merged (-> (merge m1 m2)
                                     (assoc :sum (reduce + (vals m1))))]
                      (smoke (select-keys merged [:x :z :sum])))]
         (smoke (reduce + (vals result))))))

  (is (reset-invariant?
       (let [lst (smoke [2 4 6 8 10 12 14 16])
             result (let [evens (->> lst (filter even?))
                          doubled (map #(* % 2) evens)
                          sum (reduce + doubled)]
                      (smoke (take-last 3 (range sum))))]
         (smoke (reduce + result)))))

  (is (reset-invariant?
       (let [x (smoke 200)
             y (let [a (->> (range 0 x 10)
                            (map #(* % 3))
                            (filter #(> % 100)))
                     sum (reduce + a)]
                 (smoke sum))]
         (smoke (- y 500)))))

  (is (reset-invariant?
       (let [m (smoke {:a 5, :b 10, :c 15})
             result (let [updated (-> m
                                      (update :a #(* % 2))
                                      (update :b #(/ % 5)))]
                      (smoke (reduce + (vals updated))))]
         (smoke (* result 10)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (->> lst
                                     (filter even?))
                          odds (->> lst
                                    (filter odd?))
                          combined (->> (concat (map #(* % 2) evens)
                                                (map #(+ % 1) odds)))]
                      (smoke (reduce + combined)))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [x (smoke 50)
             y (let [a (->> (range 1 x)
                            (filter #(zero? (mod % 4))))
                     b (map #(* % 3) a)
                     c (reduce + b)]
                 (smoke c))]
         (smoke (+ y 100)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (->> (range 1 x)
                            (filter #(zero? (mod % 3))))
                     b (map #(* % 2) a)
                     c (reduce + b)]
                 (smoke c))]
         (smoke (/ y 10)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (->> (range x)
                            (filter #(zero? (mod % 5)))
                            (map #(* % 3))
                            (reduce +))]
                 (smoke a))
             z (if (> y 1000)
                 (smoke (/ y 10))
                 (smoke (* y 2)))]
         (smoke (+ z 50)))))

  (is (reset-invariant?
       (let [m (smoke {:a 10 :b 20 :c 30})
             result (let [updated (-> m
                                      (assoc :d 40)
                                      (update :a #(* % 2))
                                      (dissoc :b))]
                      (smoke (reduce + (vals updated))))]
         (smoke (* result 2)))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5 6 7 8 9 10])
             result (let [evens (->> lst
                                     (filter even?)
                                     (map #(* % 2)))
                          odds (->> lst
                                    (filter odd?)
                                    (map #(+ % 1)))]
                      (smoke (reduce + (concat evens odds))))]
         (smoke (/ result 3)))))

  (is (reset-invariant?
       (let [x (smoke 200)
             y (let [a (->> (range 0 x 10)
                            (map #(* % 2))
                            (filter #(> % 50))
                            (partition 2))]
                 (smoke (reduce + (map first a))))]
         (smoke (- y 100)))))

  (is (reset-invariant?
       (let [data (smoke [5 10 15 20 25 30])
             result (let [groups (group-by #(> % 15) data)
                          large (get groups true)
                          small (get groups false)]
                      (smoke (+ (reduce + small)
                                (reduce * large))))]
         (smoke (/ result 2)))))

  (is (reset-invariant?
       (let [m1 (smoke {:x 5 :y 10})
             m2 (smoke {:z 15 :w 20})
             result (let [merged (merge m1 m2)
                          keys (->> merged
                                    (vals)
                                    (filter #(> % 10)))]
                      (smoke (reduce + keys)))]
         (smoke (* result 3)))))

  (is (reset-invariant?
       (let [lst (smoke [2 4 6 8 10 12 14 16])
             result (let [slices (->> lst
                                      (drop 2)
                                      (take 4)
                                      (map #(* % 3)))]
                      (smoke (reduce + slices)))]
         (smoke (inc result)))))

  (is (reset-invariant?
       (let [x (smoke 75)
             y (let [a (->> (range x)
                            (filter #(zero? (mod % 7)))
                            (map #(* % 2))
                            (into []))]
                 (smoke (reduce + a)))]
         (smoke (/ y 5)))))

  (is (reset-invariant?
       (let [data (smoke {:a [1 2] :b [3 4]})
             result (let [merged (->> data
                                      (vals)
                                      (apply concat)
                                      (map inc))]
                      (smoke (reduce + merged)))]
         (smoke (* result 2)))))

  (is (reset-invariant?
       (let [x (smoke 100)
             y (let [a (->> (range x)
                            (filter #(zero? (mod % 7))))
                     b (map #(* % 3) a)
                     c (reduce + b)]
                 (smoke c))]
         (smoke (/ y 10)))))

  (is (reset-invariant?
       (let [n (smoke 10)
             result (loop [i 1
                           total 0]
                      (if (> i n)
                        (smoke total)
                        (recur (inc i) (+ total i))))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke 5)
             result (loop [i n
                           acc 1]
                      (if (zero? i)
                        (smoke acc)
                        (recur (dec i) (* acc i))))]
         (smoke result))))

  (is (reset-invariant?
       (let [lst (smoke [1 2 3 4 5])
             result (loop [in lst
                           out []]
                      (if (empty? in)
                        (smoke out)
                        (recur (rest in) (conj out (first in)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke 20)
             result (loop [i 0
                           sum 0]
                      (if (> i n)
                        (smoke sum)
                        (recur (inc i) (if (even? i) (+ sum i) sum))))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke 8)
             result (loop [a 0
                           b 1
                           count 0]
                      (if (= count n)
                        (smoke a)
                        (recur b (+ a b) (inc count))))]
         (smoke result))))

  (is (reset-invariant?
       (let [coll (smoke [1 2 3 2 4 2 5])
             target (smoke 2)
             result (loop [remaining coll
                           cnt 0]
                      (if (empty? remaining)
                        (smoke cnt)
                        (recur (rest remaining)
                               (if (= (first remaining) target)
                                 (inc cnt)
                                 cnt))))]
         (smoke result))))

  (is (reset-invariant?
       (let [result (loop [i (smoke 1)
                           table []]
                      (if (> i 3)
                        (smoke table)
                        (recur (inc i)
                               (loop [j 1
                                      row []]
                                 (if (> j 3)
                                   (conj table row)
                                   (recur (inc j)
                                          (conj row (* i j))))))))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke 17)
             result (loop [i 2]
                      (cond
                        (>= i n) (smoke true)
                        (zero? (mod n i)) (smoke false)
                        :else (recur (inc i))))]
         (smoke result))))

  (is (reset-invariant?
       (let [a (smoke [1 3 5])
             b (smoke [2 4 6])
             result (loop [x a
                           y (smoke b)
                           out []]
                      (if (or (empty? x) (empty? y))
                        (smoke out)
                        (recur (rest x)
                               (rest y)
                               (conj out (first x) (first y)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [lst (smoke [10 20 30 40])
             result (loop [remaining lst
                           sum 0
                           out []]
                      (if (empty? remaining)
                        (smoke out)
                        (let [new-sum (+ sum (first remaining))]
                          (recur (rest remaining)
                                 new-sum
                                 (conj out new-sum)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [lst (smoke [3 7 2 9 4 1])
             result (loop [remaining lst
                           max-val (first lst)]
                      (if (empty? remaining)
                        (smoke max-val)
                        (recur (rest remaining)
                               (if (> (first remaining) max-val)
                                 (first remaining)
                                 max-val))))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke 5)
             result (loop [i 0
                           squares []]
                      (if (> i n)
                        (smoke squares)
                        (recur (inc i)
                               (conj squares (* i i)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (smoke 0)
             result (try
                      (smoke (/ x y))
                      (catch ArithmeticException _
                        (smoke :division-error)))]
         (smoke result))))

  (is (reset-invariant?
       (let [input (smoke "abc")
             result (try
                      (smoke (Integer/parseInt input))
                      (catch NumberFormatException _
                        (smoke :invalid-number)))]
         (smoke result))))

  (is (reset-invariant?
       (let [coll (smoke [1 2 3])
             index (smoke 5)
             result (try
                      (smoke (nth coll index))
                      (catch IndexOutOfBoundsException _
                        (smoke :out-of-bounds)))]
         (smoke result))))

  (is (reset-invariant?
       (let [nil-fn (smoke nil)
             result (try
                      (smoke (.toString nil-fn))
                      (catch NullPointerException _
                        (smoke :nil-invocation)))]
         (smoke result))))

  (is (reset-invariant?
       (let [arr (smoke (int-array [1 2 3]))
             index (smoke 5)
             result (try
                      (smoke (aget arr index))
                      (catch ArrayIndexOutOfBoundsException _
                        (smoke :invalid-index)))]
         (smoke result))))

  (is (reset-invariant?
       (let [s (smoke "clojure")
             start (smoke 10)
             end (smoke 3)
             result (try
                      (smoke (subs s start end))
                      (catch StringIndexOutOfBoundsException _
                        (smoke :invalid-substring)))]
         (smoke result))))

  (is (reset-invariant?
       (let [m (smoke {:a 1 :b 2})
             key (smoke :c)
             result (try
                      (smoke (get m key (throw (Exception. "Key not found"))))
                      (catch Exception _
                        (smoke :missing-key)))]
         (smoke result))))

  (is (reset-invariant?
       (let [n (smoke -10)
             result (try
                      (smoke (first n))
                      (catch IllegalArgumentException _
                        (smoke :invalid-input)))]
         (smoke result))))

  (is (reset-invariant?
       (let [coll (smoke 12345)          ; Not a collection
             result (try
                      (smoke (first coll))
                      (catch IllegalArgumentException _
                        (smoke :not-a-collection)))]
         (smoke result))))

  (is (reset-invariant?
       (let [dividend (smoke 100)
             divisor (smoke "two")       ; Invalid type
             result (try
                      (smoke (/ dividend divisor))
                      (catch ClassCastException _
                        (smoke :type-mismatch)))]
         (smoke result))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (smoke 20)
             p (java.awt.Point. x y)]
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (smoke 20)
             p (java.awt.Point. x y)]
         (.x ^java.awt.Point (smoke p)))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (smoke 20)
             p (java.awt.Point. x y)]
         (.getY ^java.awt.Point (smoke p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 5 15)]
         (smoke (.y p)))))

  (is (reset-invariant?
       (let [p1 (java.awt.Point. 1 2)
             p2 (java.awt.Point. 3 4)
             result (.distance p1 p2)]
         (smoke result))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 7 8)]
         (smoke (.getLocation p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 10 20)]
         (smoke (.toString p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 3 4)]
         (smoke (.equals p (java.awt.Point. 3 4))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 5 5)]
         (smoke (.setLocation p 10 15))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 2 3)]
         (smoke (.move p 5 5))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 1 1)]
         (smoke (.translate p 4 4))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (smoke (.distanceSq p (java.awt.Point. 3 4))))))

  (is (reset-invariant?
       (let [points (smoke [(java.awt.Point. 1 2)
                            (java.awt.Point. 3 4)
                            (java.awt.Point. 5 6)])
             result (loop [remaining points
                           sum-x 0
                           sum-y 0]
                      (if (empty? remaining)
                        (smoke (java.awt.Point. sum-x sum-y))
                        (let [p (first remaining)]
                          (recur (rest remaining)
                                 (+ sum-x (.x p))
                                 (+ sum-y (.y p))))))]
         (smoke result))))

  (is (reset-invariant?
       (let [x (smoke 10)
             y (smoke 0)
             result (try
                      (smoke (java.awt.Point. x (/ x y)))
                      (catch ArithmeticException _
                        (smoke :division-error)))]
         (smoke result))))

  (is (reset-invariant?
       (let [points (smoke [(java.awt.Point. 1 2)
                            (java.awt.Point. 3 4)
                            (java.awt.Point. 5 6)])
             result (loop [remaining points
                           max-dist 0]
                      (if (empty? remaining)
                        (smoke max-dist)
                        (let [p (first remaining)
                              dist (.distance p (java.awt.Point. 0 0))]
                          (recur (rest remaining)
                                 (if (> dist max-dist) dist max-dist)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [input (smoke "10,20")
             result (try
                      (let [[x y] (map #(Integer/parseInt %)
                                       (clojure.string/split input #","))]
                        (smoke (java.awt.Point. x y)))
                      (catch NumberFormatException _
                        (smoke :invalid-input)))]
         (smoke result))))

  (is (reset-invariant?
       (let [points (smoke [(java.awt.Point. 1 2)
                            (java.awt.Point. 3 4)
                            (java.awt.Point. 5 6)])
             result (loop [remaining points
                           translated []]
                      (if (empty? remaining)
                        (smoke translated)
                        (let [p (first remaining)]
                          (.translate p 10 10)
                          (recur (rest remaining)
                                 (conj translated p)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [p (smoke (java.awt.Point. 5 5))
             result (try
                      (smoke (.setLocation p "10" "15")) ; Invalid arguments
                      (catch IllegalArgumentException _
                        (smoke :invalid-location)))]
         (smoke result))))

  (is (reset-invariant?
       (let [points (smoke [(java.awt.Point. 1 2)
                            (java.awt.Point. 3 4)
                            (java.awt.Point. 5 6)])
             result (loop [remaining points
                           closest (first points)]
                      (if (empty? remaining)
                        (smoke closest)
                        (let [p
                              (first remaining)
                              dist
                              (.distance p (java.awt.Point. 0 0))
                              closest-dist
                              (.distance closest (java.awt.Point. 0 0))]
                          (recur (rest remaining)
                                 (if (< dist closest-dist) p closest)))))]
         (smoke result))))

  (is (reset-invariant?
       (let [p (smoke (java.awt.Point. 3 4))
             result (try
                      (smoke (.move p 10 10))
                      (catch Exception _
                        (smoke :error-moving-point)))]
         (smoke result))))

  (is (reset-invariant?
       (let [points (smoke [(java.awt.Point. 1 2)
                            (java.awt.Point. 3 4)
                            (java.awt.Point. 5 6)])
             result (loop [remaining points
                           sum (java.awt.Point. 0 0)]
                      (if (empty? remaining)
                        (smoke sum)
                        (let [p (first remaining)]
                          (.translate sum (.x p) (.y p))
                          (recur (rest remaining) sum))))]
         (smoke result))))

  (is (reset-invariant?
       (let [p (smoke (java.awt.Point. 2 3))
             result (try
                      (smoke (.distanceSq p (java.awt.Point. 5 7)))
                      (catch Exception _
                        (smoke :error-calculating-distance)))]
         (smoke result))))

  (is (reset-invariant?
       (with-local-vars [sum 0]
         (doseq [x (smoke [1 2 3 4 5])]
           (var-set sum (+ (var-get sum) x)))
         (smoke (var-get sum)))))

  (is (reset-invariant?
       (with-local-vars [count 0]
         (doseq [n (smoke [1 2 3 4 5 6])]
           (when (even? n)
             (var-set count (inc (var-get count)))))
         (smoke (var-get count)))))

  (is (reset-invariant?
       (with-local-vars [max-val nil]
         (doseq [x (smoke [3 1 4 1 5 9 2])]
           (when (or (nil? (var-get max-val)) (> x (var-get max-val)))
             (var-set max-val x)))
         (smoke (var-get max-val)))))

  (is (reset-invariant?
       (with-local-vars [sb (StringBuilder.)]
         (doseq [s (smoke ["a" "b" "c" "d"])]
           (.append (var-get sb) s))
         (smoke (.toString (var-get sb))))))

  (is (reset-invariant?
       (with-local-vars [state :init]
         (doseq [input (smoke [:start :process :end])]
           (var-set state (case input
                            :start :ready
                            :process :working
                            :end :done)))
         (smoke (var-get state)))))

  (is (reset-invariant?
       (with-local-vars [acc 1
                         n (smoke 5)]
         (while (> (var-get n) 1)
           (var-set acc (* (var-get acc) (var-get n)))
           (var-set n (dec (var-get n))))
         (smoke (var-get acc)))))

  (is (reset-invariant?
       (with-local-vars [x 0
                         y 0]
         (doseq [move (smoke [[1 0] [0 1] [1 1] [-1 0]])]
           (var-set x (+ (var-get x) (first move)))
           (var-set y (+ (var-get y) (second move))))
         (smoke [(var-get x) (var-get y)]))))

  (is (reset-invariant?
       (with-local-vars [a 0
                         b 1
                         n (smoke 6)]
         (dotimes [_ (var-get n)]
           (let [temp (var-get a)]
             (var-set a (var-get b))
             (var-set b (+ temp (var-get b)))))
         (smoke (var-get a)))))

  (is (reset-invariant?
       (with-local-vars [result []]
         (doseq [x (smoke [10 25 30 45 50])]
           (when (> x 20)
             (var-set result (conj (var-get result) x)))
           (smoke (var-get result))))))

  (is (reset-invariant?
       (with-local-vars [flag true]
         (doseq [_ (smoke (range 3))]
           (var-set flag (not (var-get flag))))
         (smoke (var-get flag)))))

  (is (reset-invariant?
       (with-local-vars [sum 0]
         (loop [nums (smoke [1 2 3 4 5])]
           (if (empty? nums)
             (smoke (var-get sum))
             (do
               (var-set sum (+ (var-get sum) (first nums)))
               (recur (rest nums))))))))

  (is (reset-invariant?
       (with-local-vars [count 0]
         (loop [nums (smoke [1 2 3 4 5 6])]
           (if (empty? nums)
             (smoke (var-get count))
             (do
               (when (even? (first nums))
                 (var-set count (inc (var-get count))))
               (recur (rest nums))))))))

  (is (reset-invariant?
       (with-local-vars [max-val nil]
         (loop [nums (smoke [3 1 4 1 5 9 2])]
           (if (empty? nums)
             (smoke (var-get max-val))
             (do
               (when (or (nil? (var-get max-val))
                         (> (first nums) (var-get max-val)))
                 (var-set max-val (first nums)))
               (recur (rest nums))))))))

  (is (reset-invariant?
       (with-local-vars [sb (StringBuilder.)]
         (loop [strings (smoke ["a" "b" "c" "d"])]
           (if (empty? strings)
             (smoke (.toString (var-get sb)))
             (do
               (.append (var-get sb) (first strings))
               (recur (rest strings))))))))

  (is (reset-invariant?
       (with-local-vars [state :init]
         (loop [inputs (smoke [:start :process :end])]
           (if (empty? inputs)
             (smoke (var-get state))
             (do
               (var-set state (case (first inputs)
                                :start :ready
                                :process :working
                                :end :done))
               (recur (rest inputs))))))))

  (is (reset-invariant?
       (with-local-vars [acc 1]
         (loop [n (smoke 5)]
           (if (<= n 1)
             (smoke (var-get acc))
             (do
               (var-set acc (* (var-get acc) n))
               (recur (dec n))))))))

  (is (reset-invariant?
       (with-local-vars [x 0
                         y 0]
         (loop [moves (smoke [[1 0] [0 1] [1 1] [-1 0]])]
           (if (empty? moves)
             (smoke [(var-get x) (var-get y)])
             (do
               (var-set x (+ (var-get x) (first (first moves))))
               (var-set y (+ (var-get y) (second (first moves))))
               (recur (rest moves))))))))

  (is (reset-invariant?
       (with-local-vars [a 0
                         b 1]
         (loop [n (smoke 6)]
           (if (zero? n)
             (smoke (var-get a))
             (do
               (let [temp (var-get a)]
                 (var-set a (var-get b))
                 (var-set b (+ temp (var-get b))))
               (recur (dec n))))))))

  (is (reset-invariant?
       (with-local-vars [result []]
         (loop [nums (smoke [10 25 30 45 50])]
           (if (empty? nums)
             (smoke (var-get result))
             (do
               (when (> (first nums) 20)
                 (var-set result (conj (var-get result) (first nums))))
               (recur (rest nums))))))))

  (is (reset-invariant?
       (with-local-vars [flag true]
         (loop [n (smoke 3)]
           (if (zero? n)
             (smoke (var-get flag))
             (do
               (var-set flag (not (var-get flag)))
               (recur (dec n))))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 1 2)]
         (set! (.x p) 10)
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 3 4)]
         (set! (.y p) 20)
         (smoke (.y p)))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 5 10)]
         (set! (.width d) 15)
         (smoke (.width d)))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 20 30)]
         (set! (.height d) 40)
         (smoke (.height d)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (set! (.x p) 5)
         (set! (.y p) 10)
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (when (< i 5)
             (set! (.x p) (inc (.x p)))
             (recur (inc i))))
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 1 2)]
         (try
           (set! (.x p) "invalid")
           (smoke :ok)
           (catch ClassCastException _
             (smoke :type-error))))))

  (is (reset-invariant?
       (let [p1 (java.awt.Point. 1 2)
             p2 (java.awt.Point. 3 4)]
         (set! (.x p1) 10)
         (set! (.y p2) 20)
         (smoke [(.x p1) (.y p2)]))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 5 5)]
         (when (> (.x p) 3)
           (set! (.x p) 0))
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 2 3)]
         (set! (.x p) (* (.x p) 2))
         (set! (.y p) (+ (.y p) 5))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (when (< i 5)
             (set! (.x p) (+ (.x p) 1))
             (recur (inc i))))
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (if (< i 10)
             (do
               (set! (.y p) (+ (.y p) 2))
               (recur (inc i)))
             (smoke (.y p)))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (when (< i 3)
             (set! (.x p) (* (.x p) 2))
             (set! (.y p) (+ (.y p) 5))
             (recur (inc i))))
         (smoke p))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 10 10)]
         (loop [i 0]
           (if (< i 4)
             (do
               (set! (.width d) (+ (.width d) 5))
               (set! (.height d) (+ (.height d) 5))
               (recur (inc i)))
             (smoke d))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 1 1)]
         (loop [i 0]
           (when (< i 5)
             (set! (.x p) (+ (.x p) (.y p)))
             (recur (inc i))))
         (smoke (.x p)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (if (< i 10)
             (do
               (set! (.x p) (+ (.x p) i))
               (recur (inc i)))
             (smoke (.x p)))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 5 5)]
         (loop [i 0]
           (when (< i 3)
             (set! (.x p) (- (.x p) 1))
             (set! (.y p) (+ (.y p) 2))
             (recur (inc i))))
         (smoke p))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 20 20)]
         (loop [i 0]
           (if (< i 5)
             (do
               (set! (.width d) (* (.width d) 2))
               (set! (.height d) (* (.height d) 2))
               (recur (inc i)))
             (smoke d))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (loop [i 0]
           (when (< i 4)
             (set! (.x p) (+ (.x p) i))
             (set! (.y p) (+ (.y p) (* i 2)))
             (recur (inc i))))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 10 10)]
         (loop [i 0]
           (if (< i 5)
             (do
               (set! (.x p) (/ (.x p) 2))
               (set! (.y p) (* (.y p) 2))
               (recur (inc i)))
             (smoke p))))))

  (is (reset-invariant?
       (quote 1)))

  (is (reset-invariant?
       (let [v (smoke [1 2 3 4])]
         (conj v (reduce * v)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. (smoke 5) 10)]
         (.distance p (java.awt.Point. 0 0)))))

  (is (reset-invariant?
       (with-local-vars [sum 0]
         (doseq [x (smoke [1.5 2.5 3.5])]
           (var-set sum (+ (var-get sum) x)))
         (var-get sum))))

  (is (reset-invariant?
       (let [f (smoke #(inc %))]
         ((comp f f f) 2))))

  (is (reset-invariant?
       (let [a (smoke 5)
             b (smoke 10)]
         (+ a b (* 2 a)))))

  (is (reset-invariant?
       (->> (range (smoke 5))
            (map inc)
            (reduce +))))

  (is (reset-invariant?
       (let [s (smoke "hello")]
         (str s " " (clojure.string/upper-case s)))))

  (is (reset-invariant?
       (with-local-vars [sum 0]
         (doseq [x (smoke [1 2 3 4])]
           (var-set sum (+ (var-get sum) x)))
         (var-get sum))))

  (is (reset-invariant?
       (let [p (java.awt.Point. (smoke 3) (smoke 4))]
         (.distance p (java.awt.Point. 0 0)))))

  (is (reset-invariant?
       (let [v (smoke [1 2 3])]
         (into v (map #(* % 2) v)))))

  (is (reset-invariant?
       (loop [i (smoke 0)
              acc 1]
         (if (> i 5)
           acc
           (recur (inc i) (* acc i))))))

  (is (reset-invariant?
       (let [f (smoke +)
             g (smoke *)]
         ((comp f g) 2 3 4))))

  (is (reset-invariant?
       (let [m (smoke {:a 1 :b 2})]
         (update m :a inc))))

  (is (reset-invariant?
       (for [x (smoke [1 2 3])]
         (* x x))))

  (is (reset-invariant?
       (some? (smoke "test"))))

  (is (reset-invariant?
       (try
         (/ 10 (smoke 0))
         (catch Exception _ :error))))

  (is (reset-invariant?
       (cond-> []
         (smoke true) (conj 1))))

  (is (reset-invariant?
       (case (smoke :key)
         :key :found
         :default)))

  (is (reset-invariant?
       (let [f (smoke +)]
         (reduce f [1 2 3]))))

  (is (reset-invariant?
       (-> (smoke 5)
           inc
           (* 2))))

  (is (reset-invariant?
       (with-local-vars [x (smoke 0)]
         (var-set x 10)
         (var-get x))))

  (is (reset-invariant?
       (java.util.ArrayList. (smoke [1 2 3]))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 5 5)]
         (loop [i 0]
           (when (< i 3)
             (set! (.width d) (+ (.width d) i))
             (set! (.height d) (- (.height d) i))
             (recur (inc i))))
         (smoke [(* (.width d) (.height d))]))))

  (is (reset-invariant?
       (with-local-vars [acc 1]
         (doseq [x (smoke [1 2 3 4])]
           (var-set acc (* (var-get acc) x)))
         (var-get acc))))

  (is (reset-invariant?
       (let [p1 (java.awt.Point. 0 0)
             p2 (java.awt.Point. 10 10)]
         (loop [i 0]
           (when (< i 5)
             (set! (.x p1) (+ (.x p1) 2))
             (set! (.y p2) (- (.y p2) 2))
             (recur (inc i))))
         (smoke [(.distance p1 p2)]))))

  (is (reset-invariant?
       (let [arr (make-array Integer/TYPE 3)]
         (aset arr 0 (smoke 5))
         (aset arr 1 10)
         (aset arr 2 (* (aget arr 0) 2))
         (smoke (vec arr)))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 100 100)]
         (dotimes [i 4]
           (set! (.x p) (quot (.x p) 2))
           (set! (.y p) (quot (.y p) 2)))
         (smoke (.distance p (java.awt.Point. 0 0))))))

  (is (reset-invariant?
       (with-local-vars [s ""]
         (doseq [c (smoke [\a \b \c \d])]
           (var-set s (str (var-get s) c)))
         (str (var-get s) "!"))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 3 4)
             d (java.awt.Dimension. 2 2)]
         (loop [i 0]
           (when (< i 3)
             (set! (.x p) (* (.x p) (.width d)))
             (set! (.y p) (* (.y p) (.height d)))
             (recur (inc i))))
         (smoke p))))

  (is (reset-invariant?
       (let [counter (smoke 0)]
         (with-local-vars [counter (smoke 0)
                           sum 0]
           (while (< (var-get sum) 20)
             (var-set sum (+ (var-get sum) 5))
             (var-set counter (inc (var-get counter)))))
         (smoke counter))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (doseq [offset (smoke [1 3 5 7])]
           (set! (.x p) (+ (.x p) offset))
           (set! (.y p) (- (.y p) offset)))
         (smoke [(* (.x p) 2) (* (.y p) 3)]))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 8 8)]
         (loop [i 0]
           (if (even? i)
             (set! (.width d) (+ (.width d) i))
             (set! (.height d) (- (.height d) i)))
           (when (< i 5)
             (recur (inc i))))
         (smoke d))))

  (is (reset-invariant?
       (let [r (java.awt.Rectangle. 0 0 10 10)]
         (dotimes [i 3]
           (set! (.width r) (+ (.width r) 5))
           (set! (.height r) (- (.height r) 2)))
         (smoke [(.x r) (.y r) (.width r) (.height r)]))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 100 100)]
         (doseq [offset [5 -3 8 -2]]
           (set! (.x p) (+ (.x p) offset))
           (set! (.y p) (- (.y p) offset)))
         (smoke p))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 1 1)]
         (loop [i 0]
           (when (< i 4)
             (set! (.width d) (* 2 (.width d)))
             (set! (.height d) (* 3 (.height d)))
             (recur (inc i))))
         (smoke d))))

  (is (reset-invariant?
       (let [p1 (java.awt.Point. 5 10)
             p2 (java.awt.Point. 20 30)]
         (set! (.x p1) (.x p2))
         (set! (.y p2) (.y p1))
         (smoke [(.x p1) (.y p1) (.x p2) (.y p2)]))))

  (is (reset-invariant?
       (let [r (java.awt.Rectangle. 2 3 15 15)]
         (when (> (.width r) 10)
           (set! (.x r) 0)
           (set! (.y r) 0))
         (smoke r))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 0 0)]
         (dotimes [i 5]
           (set! (.x p) (+ (.x p) i))
           (smoke (.x p))))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 100 50)]
         (loop [i 0]
           (if (< i 3)
             (do
               (set! (.width d) (/ (.width d) 2))
               (set! (.height d) (* (.height d) 2))
               (recur (inc i)))
             (smoke d))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 10 20)]
         (set! (.x p) (min 15 (.x p)))
         (set! (.y p) (max 25 (.y p)))
         (smoke p))))

  (is (reset-invariant?
       (let [r (java.awt.Rectangle. 5 (smoke 5) 20 10)]
         (dotimes [_ 2]
           (set! (.x r) (+ (.x r) (smoke 3)))
           (set! (.y r) (- (.y r) 1))
           (set! (.width r) (inc (.width (smoke r)))))
         (smoke r))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 3 4)]
         (loop [i 0]
           (when (< i 2)
             (set! (.x p) (* (.x p) (.x p)))
             (set! (.y p) (+ (.y p) (.y p)))
             (recur (inc i))))
         (smoke p))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 2 3)]
         (set! (.x p) (inc (.x p)))
         (set! (.y p) (* (.y p) 2))
         (smoke [(.x p) (.y p)]))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. 5 5)]
         (loop [i 0]
           (when (< i 3)
             (set! (.width (smoke d)) (+ (.width d) i))
             (set! (.height d) (- (.height d) i))
             (recur (inc i))))
         (smoke d))))

  (is (reset-invariant?
       (let [p (java.awt.Point. (smoke 100) 100)]
         (dotimes [i 4]
           (set! (.x p) (quot (.x (smoke p)) 2))
           (smoke (.x p))))))

  (is (reset-invariant?
       (let [p1 (java.awt.Point. 1 2)
             p2 (java.awt.Point. 3 4)]
         (set! (.x p1) (.x p2))
         (set! (.y p2) (.y p1))
         (smoke [p1 p2]))))

  (is (reset-invariant?
       (let [d (java.awt.Dimension. (smoke 10) 20)]
         (loop [i 0]
           (if (< i 5)
             (do
               (set! (.width (smoke d)) (inc (.width d)))
               (set! (.height d) (dec (.height (smoke d))))
               (recur (inc i)))
             (smoke d))))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 2 3)]
         (set! (.x p) (smoke (inc (.x p))))
         (set! (.y p) (smoke (* (.y p) 2)))
         (smoke [(.x p) (.y p)]))))

  (is (reset-invariant?
       (let [p (java.awt.Point. 5 10)]
         (loop [i 3]
           (when (pos? i)
             (set! (.x p) (* (.x p) i))
             (set! (.y p) (+ (.y p) i))
             (recur (dec (smoke i)))))
         (smoke p)))))

;;;; * Scope preservation

#_{:clj-kondo/ignore
   [:redundant-let
    :redundant-do]}
(deftest test-scope-preservation
  (reset
   (let [a (smoke 0)]
     (do
       (let [a (smoke 1)]
         (is (= a 1)))
       (let [a (smoke 2)]
         (do
           (let [a (smoke 3)]
             (is (= a 3)))
           (let [a (smoke 4)]
             (is (= a 4)))
           (is (= a 2))))
       (is (= a 0)))))

  (reset
   (let [a
         (do
           (let [b (do (smoke 1))]
             (is (= b 1))
             2))]
     (is (= a 2)))))

;;;; * Direct-target recur

(deftest test-direct-target-recur
  (is (= (reset
           (loop [n 0]
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
           (loop [n 0]
             (if false
               (smoke nil)
               42)
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
           (loop [n 0]
             (try
               (if false
                 (smoke nil)
                 42)
               (catch Exception _
                 (if false
                   (smoke 42)
                   nil)))
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))
  
  (is (= (reset
           (loop [n 0]
             (case n ;; Calls the continuation only twice. Should not overflow.
               10 (smoke 42) 
               20 (smoke 42)
               10)
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
           (loop [n 0]
             (try
               (case n
                 10 (smoke 42)
                 20 (smoke 42)
                 10)
               (catch Exception _
                 (if false
                   (smoke 42)
                   nil)))
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
           (loop [n 0]
             (loop [x 0]
               (try
                 (case n
                   10 (smoke 42)
                   20 (smoke 42)
                   10)
                 (catch Exception _
                   (if false
                     (smoke 42)
                     nil)))
               (when (< x 100)
                 (recur (inc x))))
             (if (< n 100)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
           (loop [n 0]
               (loop [x 0]
                 (try
                   (case n
                     10 (smoke 42)
                     20 (smoke 42)
                     10)
                   (catch Exception _
                     (if false
                       (smoke 42)
                       nil)))
                 (when (< x 100)
                   (recur (inc x))))
               (when false
                 (smoke 0))
             (if (< n 100)
               (recur (inc n))
               (smoke 42))))
         42))

  (is (= (reset
          (loop [n 0]
            (loop [x 0]
              (try
                (case n
                  10 (smoke 42)
                  20 (smoke 42)
                  10)
                (catch Exception _
                  (if false
                    (smoke 42)
                    nil)))
              (when (< x 100)
                (recur (inc x))))
            (when (= n 10)
              (smoke 0))
            (if (< n 100)
              (recur (inc n))
              (smoke 42))))
         42))

  (is (= (reset
          (loop [n 0]
            (loop [x 0]
              (try
                (case n
                  10 (smoke 42)
                  20 (smoke 42)
                  10)
                (catch Exception _
                  (if false
                    (smoke 42)
                    nil)))
              (when (< x 100)
                (recur (inc x))))
            (when (= n 10)
              (smoke 0))
            (loop [x 0]
              (try
                (case n
                  10 (smoke 42)
                  20 (smoke 42)
                  10)
                (catch Exception _
                  (if false
                    (smoke 42)
                    nil)))
              (when (< x 100)
                (recur (inc x))))
            (if (< n 100)
              (recur (inc n))
              (smoke 42))))
         42))

  (is (= (reset
          (loop [n 0]
            (loop [x 0]
              (try
                (when false
                  (smoke 0))
                (case 0
                  10 (smoke 42)
                  20 (smoke 42)
                  10)
                (catch Exception _
                  (if false
                    (smoke 42)
                    nil)))
              (when (< x 100)
                (recur (inc x))))
            (loop [x 0]
              (when false
                (smoke 0))
              (try
                (case n
                  10 (smoke 42)
                  20 (smoke 42)
                  10)
                (catch Exception _
                  (if false
                    (smoke 42)
                    nil)))
              (when (< x 100)
                (recur (inc x))))
            (if (< n 100)
              (recur (inc n))
              (smoke 42))))
         42))
(reset-invariant?
   (loop [n 0]
     (loop [x 0]
       (try
         (when false
           (smoke 0))
         (case 0
           10 (smoke 42)
           20 (smoke 42)
           10)
         (catch Exception _
           (if false
             (smoke 42)
             nil)))
       (when (< x 100)
         (recur (inc x))))
     (loop [x 0]
       (try
         (when false
           (smoke 0))
         (case 0
           10 (smoke 42)
           20 (smoke 42)
           10)
         (catch Exception _
           (if false
             (smoke 42)
             nil)))
       (when (< x 100)
         (recur (inc x))))
     (loop [x 0]
       (try
         (when false
           (smoke 0))
         (case 0
           10 (smoke 42)
           20 (smoke 42)
           10)
         (catch Exception _
           (if false
             (smoke 42)
             nil)))
       (when (< x 100)
         (recur (inc x))))
     (loop [x 0]
       (when false
         (smoke 0))
       (try
         (case n
           10 (smoke 42)
           20 (smoke 42)
           10)
         (catch Exception _
           (if false
             (smoke 42)
             nil)))
       (when (< x 100)
         (recur (inc x))))
     (if (< n 100)
       (recur (inc n))
       (smoke 42))))

  (is (= (reset
           (loop [n 0]
             (try
               (if false
                 (smoke nil)
                 42)
               (catch Exception _
                 (if false
                   (smoke 42)
                   nil))
               (finally
                 (if false
                   (smoke 42)
                   nil)))
             (if (< n 100000)
               (recur (inc n))
               (smoke 42))))
         42))

  (let [do-loop
        (fn-shift
          [n]
          (if (< n 100000)
            (recur (inc n))
            (smoke 42)))]
    (is (= (reset
             (do-loop 0))
           42)))

  (is (= (reset
           (loop [n 0]
             (if (< n 100000)
               (if (= (mod n 3) 0)
                 (if false
                   (smoke nil)
                   (recur (inc n)))
                 (recur (inc (inc n))))
               (smoke 42))))
         42))

  (let [do-loop
        (fn-shift
          [n]
          (do
            (when false (smoke nil))
            (if (< n 100000)
              (recur (inc n))
              (smoke 42))))]
    (is (= (reset
             (do-loop 0))
           42)))

  (is (= (reset
           (loop [a 0
                  b 1]
             (if (> a 100000)
               (smoke b)
               (recur b (+ a b)))))
         196418)))
