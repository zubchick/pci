(defproject pci "0.0.1"
  :description "Examples for \"Programming Collective Intelligence\""
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]]
  :dev-dependencies [[clojure-source "1.3.0"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"
             "-Xmx1g" "-Xms256m"])
