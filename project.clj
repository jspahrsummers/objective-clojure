(defproject objclj "0.1.0-SNAPSHOT"
  :description "Clojure to Objective-C compiler"
  :url "https://github.com/jspahrsummers/objective-clojure"
  :license "Public Domain"
  :main objclj.repl
  :dependencies [[org.clojure/clojure "1.4.0"] [org.clojure/core.match "0.2.0-alpha10"]]
  :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  :git-dependencies [["git://github.com/richard-lyman/amotoen.git", "50da08b6d45149112d69378d6751dfeb0657b339"]]
  :source-paths ["src" ".lein-git-deps/amotoen/src/"])
