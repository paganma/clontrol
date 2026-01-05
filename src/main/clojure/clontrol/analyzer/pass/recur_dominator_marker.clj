(ns clontrol.analyzer.pass.recur-dominator-marker
  "Marks the nodes dominating a `recur` expression in an AST."
  (:require
   [clontrol.analyzer.node
    :as node]
   [clontrol.analyzer.pass.direct-marker
    :refer [mark-direct]]))

(defn mark-intermediate
  [return node]
  (if (:recur-dominator? node)
    (return node)
    (let [node (assoc node :recur-dominator? true)]
      (if (node/intermediate? node)
        (return node)
        #(node/update-children return mark-intermediate node)))))

(defn mark-expression
  [return node]
  (node/update-children
   (fn [{operation :op
         :as node}]
     (if (or (= operation :recur)
             (node/some-child?
              (fn [child-node]
                (and (not (= (:op child-node) :loop))
                     (node/tail? child-node)
                     (:recur-dominator? child-node)))
              node))
       (mark-intermediate return node)
       (return node)))
   (fn [return child-node]
     (if (node/tail? child-node)
       #(mark-expression return child-node)
       (return child-node)))
   node))

(def ^:dynamic *mark*
  "The `mark` function used by `mark-pure`.

  Default value is [[mark-expression]]."
  mark-expression)

(defn mark-recur-dominator
  {:pass-info
   {:depends #{#'mark-direct}
    :walk :none}}
  ([node]
   (trampoline mark-recur-dominator identity node))
  ([return node]
   (*mark* return node)))
