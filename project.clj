(defproject t6/from-scala "0.2.1"
  :description "A Scala interop library for Clojure"
  :url "https://github.com/t6/from-scala"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :profiles {:scala2.10 {:dependencies [[org.scala-lang/scala-library "2.10.4"]]}
             :dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.scala-lang/scala-library "2.11.6"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje-doc "0.0.24"]
                             [lein-midje "3.1.3"]]}}
  :documentation {:files {"doc/index"
                          {:input "test/t6/from_scala/guide.clj"
                           :title "from-scala"
                           :sub-title "A Scala interop library for Clojure"
                           :author "Tobias Kortkamp"
                           :email  "tobias.kortkamp@gmail.com"}}}
  :dependencies [[cats "0.3.2"
                  :exclusions [com.keminglabs/cljx
                               org.clojure/clojurescript]]
                 [potemkin "0.3.12"]]
  :aliases {"test-all" ["do" "midje," "with-profile" "+scala2.10" "midje"]})
