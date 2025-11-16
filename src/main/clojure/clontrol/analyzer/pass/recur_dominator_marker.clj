(ns clontrol.analyzer.pass.recur-dominator-marker
  "Marks the nodes dominating a `recur` expression in an AST."
  (:require
   [clontrol.analyzer.node
    :as node]))

(declare mark-tail)

(defn mark-intermediate
  [return node]
  (node/update-children
   return
   (fn [return child-node]
     (if (node/tail? child-node)
       (mark-tail return child-node)
       (if (:recur-dominator? node)
         (mark-intermediate return (assoc child-node :recur-dominator? true))
         (mark-intermediate return child-node))))
   node))

(defn mark-tail
  [return node]
  (node/update-children
   (fn [node]
     (if (= (:op node) :recur)
       (mark-intermediate return (assoc node :recur-dominator? true))
       (if (node/some-child? :recur-dominator? node)
         (mark-intermediate return (assoc node :recur-dominator? true))
         (return node))))
   (fn [return child-node]
     (if (node/tail? child-node)
       (mark-tail return child-node)
       (mark-intermediate return child-node)))
   node))

(defn mark-expression
  [return node]
  (if (node/tail? node)
    (mark-tail return node)
    (mark-intermediate return node)))

(defn mark-recur-dominator
  {:pass-info
   {:walk :none}}
  ([node]
   (trampoline mark-recur-dominator identity node))
  ([return node]
   (mark-expression return node)))
