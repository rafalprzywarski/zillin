(defproject zillin "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [proto-repl "0.3.1"]]
  :main ^:skip-aot zillin.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
