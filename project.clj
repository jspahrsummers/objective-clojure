(defproject objclj "0.1.0-SNAPSHOT"
  :description "Clojure to Objective-C compiler"
  :url "https://github.com/jspahrsummers/objective-clojure"
  :license "Public Domain"
  :main objclj.repl
  :dependencies [[org.clojure/clojure "1.4.0"] [org.clojure/core.match "0.2.0-alpha10"] [org.van-clj/zetta-parser "0.0.3"]]
  :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  :source-paths ["src"])
