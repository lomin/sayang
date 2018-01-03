(defproject me.lomin/sayang "0.2.0"
  :description "Complects the definition of a function with its specification."
  :url "https://github.com/lomin/sayang"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.rpl/specter "1.1.0"]]
  :test-selectors {:default     (constantly true)
                   :unit        :unit
                   :focused     :focused}
  :profiles {:dev {:jvm-opts ["-Dme.lomin.sayang.activate=true"]
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/spec.alpha "0.1.143"]
                                  [orchestra "2017.11.12-1"]
                                  [org.clojure/clojure "1.9.0"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.21.1"]
                             [lein-ancient "0.6.14"]
                             [jonase/eastwood "0.2.5"]
                             [lein-cljfmt "0.5.7"]]}})
