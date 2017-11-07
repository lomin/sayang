(defproject sayang "0.1.0"
  :description "Complects the definition of a function with its specification."
  :url "https://github.com/lomin/sayang"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-beta3"]
                 [com.rpl/specter "1.0.4"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.21.1"]
                             [lein-ancient "0.6.14"]
                             [jonase/eastwood "0.2.5"]
                             [lein-cljfmt "0.5.7"]]}})
