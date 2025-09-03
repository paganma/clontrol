(ns clontrol.analyzer.pass.meta-reader
  "Utilities to read meta-tags from an AST node")

(defmulti read-meta
  "Reads the value of `meta-key` in `node`'s meta-map."
  (fn [node _]
    (:op node)))

(defn read-form-meta
  [{form :form} meta-key]
  (meta-key (meta form)))

(defmethod read-meta
  :default
  [node meta-key]
  (read-form-meta node meta-key))

(defn read-local-meta
  [{local-symbol :form
    {locals-environment :locals} :env}
   meta-key]
  (or
   (-> local-symbol
       :form meta meta-key)
   (-> (local-symbol locals-environment)
       :form meta meta-key)))

(defmethod read-meta
  :local
  [local-node meta-key]
  (read-local-meta local-node meta-key))

(defn read-var-meta
  [{var-meta :meta}
   meta-key]
  (meta-key var-meta))

(defmethod read-meta
  :var
  [local-node meta-key]
  (read-var-meta local-node meta-key))
