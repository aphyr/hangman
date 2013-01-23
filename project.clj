(defproject hangman "0.1.0-SNAPSHOT"
  :description "An overpowered hangman AI."
  :url "http://github.com/aphyr"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["src/hangman/"]
  :main hangman.bin
  :aot [hangman.bin]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-wallhack "1.0"]
                 [org.clojure/core.cache "0.6.2"]
                 [org.clojure/core.match "0.2.0-alpha11"]
                 [org.clojure/math.numeric-tower "0.0.1"]])

