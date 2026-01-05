(ns clontrol.analyzer.node
  "Operations for querying and transforming AST nodes")

(defn node?
  "Returns `true` if `value` is a map conforming to a `node`, `false`
  otherwise."
  [value]
  (and
   (map? value)
   (contains? value :op)
   (map? (:env value))
   (contains? value :form)))

(defn tail?
  "Returns `true` if the `node` sits in the tail (or return) position."
  [{{context :context} :env}]
  (= context :ctx/return))

(defn intermediate?
  "Returns `true` if the `node` sits in an intermediate (not [[tail?]]) position."
  [node]
  (not (tail? node)))

(defn read-children
  "Returns the vector of `node`'s child nodes."
  ([{child-keys :children
     :as node}]
   (read-children node [] child-keys))
  ([node child-nodes child-keys]
   (if (seq child-keys)
     (let [[child-key & child-keys'] child-keys
           child (child-key node)
           child-nodes'
           (if (vector? child)
             (into child-nodes child)
             (conj child-nodes child))]
       (recur node child-nodes' child-keys'))
     child-nodes)))

(defn every-child?
  "Returns `true` if `predicate` is `true` for every child of `node`,
  `false` otherwise."
  [predicate node]
  (every? predicate (read-children node)))

(defn every-tail?
  "Returns `true` if `predicate` is `true` for every child of `node` in tail
  position, `false` otherwise."
  [predicate node]
  (every-child?
   (fn [child-node]
     (or (not (tail? child-node))
         (predicate child-node)))
   node))

(defn some-child?
  "Returns `true` if `predicate` is `true` for some child of `node`,
  `false` otherwise."
  [predicate node]
  (some predicate (read-children node)))

(defn update-child
  "Applies a CPS `transform` to a child of `node` referred by `child-key` and
  yields the transformed `node` to `return`. If the child referenced by
  `child-key` is a vector, this function applies the `transform` to each element
  of the vector."
  ([return transform node child-key]
   (let [child (child-key node)]
     (letfn [(transform-each
               ([return nodes]
                (transform-each return nodes []))
               ([return nodes transformed-nodes]
                (if (seq nodes)
                  (let [[node & nodes'] nodes]
                    (transform
                     (fn [transformed-node]
                       (let [transformed-nodes'
                             (conj transformed-nodes transformed-node)]
                         #(transform-each return nodes' transformed-nodes')))
                     node))
                  (return transformed-nodes))))]
       (let [transform-child (if (vector? child) transform-each transform)]
         (transform-child
          (fn [child']
            (return (assoc node child-key child')))
          child))))))

(defn update-children
  "Applies a CPS `transform` to every child of `node`, yielding the final
  transformed `node` to `return`."
  ([return
    transform
    {child-keys :children
     :as node}]
   (update-children return transform node child-keys))
  ([return transform node child-keys]
   (if (seq child-keys)
     (let [[child-key & child-keys'] child-keys]
       (update-child
        (fn [node']
          #(update-children return transform node' child-keys'))
        transform
        node
        child-key))
     (return node))))

(defn update-tails
  "Applies a CPS `transform` to every child of `node` in tail position, yielding
  the final transformed `node` to `return`."
  [return transform node]
  (update-children
   return
   (fn [return node]
     (if (tail? node)
       (transform return node)
       (return node)))
   node))
