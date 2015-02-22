(defproject cljminecraft "1.0.6-SNAPSHOT"
  :description "Clojure for Bukkit Minecraft"
  :dependencies [[org.clojure/clojure         "1.5.1"]
                 [org.clojure/tools.logging   "0.2.6"]
                 [org.clojure/tools.nrepl     "0.2.7"]
                 [clojure-complete            "0.2.3"]
                 [cheshire                    "5.2.0"]
                 [org.reflections/reflections "0.9.8"]
                 [mud "0.1.0-SNAPSHOT"]
                 [clj-native "0.9.3"]
                 [net.java.dev.jna/jna "4.1.0"]]

  :resource-paths ["resources/bukkit-1.8-R0.1-SNAPSHOT.jar"
                   "resources/overtone-logo.png"
                   "resources/minecraft.overtone-0.10-SNAPSHOT-standalone.jar"]
  :profiles {:dev {:dependencies []}}
  :javac-options [ "-d" "classes/" "-source" "1.6" "-target" "1.6"]
  :java-source-paths ["javasrc"])
