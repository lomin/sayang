(defproject me.lomin/sayang "0.3.0"
  :description "Complects the definition of a Clojure(Script) function with its specification."
  :url "https://github.com/lomin/sayang"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.rpl/specter "1.1.1"]]
  :test-selectors {:default     (constantly true)
                   :unit        :unit
                   :focused     :focused}
  :profiles {:dev {:source-paths ["src" "repl"]
                   :jvm-opts ["-Dme.lomin.sayang.*activate*=true"]
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [orchestra "2018.08.19-1"]
                                  [org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojurescript "1.10.339"]
                                  [com.bhauman/figwheel-main "0.1.9"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.21.1"]
                             [lein-ancient "0.6.14"]
                             [jonase/eastwood "0.2.5"]
                             [lein-cljfmt "0.5.7"]]}})