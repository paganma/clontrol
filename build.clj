(ns build
  (:require
   [clojure.tools.build.api
    :as build]))

(def lib 'paganma/clontrol)

(def version (format "0.1.%s" (build/git-count-revs nil)))

(def class-dir "target/classes")

(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def basis (delay (build/create-basis {:project "deps.edn"})))

(println (:paths @basis))

(defn clean [_]
  (build/delete {:path "target"}))

(defn classes
  [_]
  (build/compile-clj
   {:basis @basis
    :class-dir class-dir
    :filter-ns '[clontrol]}))

(defn jar [_]
  (build/write-pom
   {:basis @basis
    :class-dir class-dir
    :lib lib
    :version version
    :src-dirs ["src/main"]})
  (build/copy-dir
   {:src-dirs ["src/main"]
    :target-dir class-dir})
  (build/jar
   {:class-dir class-dir
    :jar-file jar-file}))
