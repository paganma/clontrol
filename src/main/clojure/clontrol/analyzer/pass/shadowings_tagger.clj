(ns clontrol.analyzer.pass.shadowings-tagger
  "Tags the shadowings introduced by nodes in an AST."
  (:require
   [clontrol.analyzer.pass.direct-marker
    :refer [mark-direct]]
   [clontrol.analyzer.node
    :as node]))

(defn find-shadowings
  "Finds the local shadowings introduced by `node` and its children."
  [{binding-nodes :bindings
    {namespace :ns
     locals :locals} :env
    :as node}]
  (let [binding-symbols
        (into #{} (map :form) binding-nodes)
        shadowed-symbols
        (into
         #{}
         (filter
          (fn [binding-symbol]
            (or (binding-symbol locals)
                (ns-resolve namespace binding-symbol))))
         binding-symbols)]
    (into
     shadowed-symbols
     (comp
      (map :shadowings)
      cat
      (filter
       (fn [binding-symbol]
         (not (binding-symbols binding-symbol)))))
     (node/read-children node))))

(defn tag
  [return node]
  (if (:direct? node)
    (return node)
    #(node/update-children
      (fn [node]
        (return (assoc node :shadowings (find-shadowings node))))
      tag
      node)))

(defn tag-shadowings
  "Recursively tags the local `:shadowings` introduced by `node`.

  Shadowings are symbols defined in the outer scope that are
  redefined (shadowed) by `node`.
  
  Only marks subtrees that are not marked as `:direct?` (see [[mark-direct]])."
  {:pass-info
   {:after #{#'mark-direct}
    :walk :none}}
  ([node]
   (trampoline tag-shadowings identity node))
  ([return node]
   (tag return node)))
