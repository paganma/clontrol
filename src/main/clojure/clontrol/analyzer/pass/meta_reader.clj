(ns clontrol.analyzer.pass.meta-reader
  "Utilities to read metadata from an AST node")

(defmulti read-meta
  "Reads `node`'s meta-map."
  (fn [node]
    (:op node)))

(defn read-form-meta
  [{form :form}]
  (meta form))

(defmethod read-meta
  :default
  [node]
  (read-form-meta node))

(defn read-local-meta
  [{local-symbol :form
    {locals-environment :locals} :env}]
  (merge
   (-> local-symbol :form meta)
   (-> (local-symbol locals-environment) :form meta)))

(defmethod read-meta
  :local
  [local-node]
  (read-local-meta local-node))

(defn read-var-meta
  [{var-meta :meta}]
  var-meta)

(defmethod read-meta
  :var
  [local-node]
  (read-var-meta local-node))
