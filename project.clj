(defproject t6/from-scala "0.1.0"
  :description "A Scala interop library for Clojure"
  :url "https://github.com/t6/from-scala"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:scala2.10 {:dependencies [[org.scala-lang/scala-library "2.10.4"]]}
             :dev {:dependencies [[org.clojure/clojure "1.6.0"]
                                  [org.scala-lang/scala-library "2.11.2"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje-doc "0.0.24"]
                             [lein-midje "3.1.3"]]}}
  :documentation {:files {"doc/index"
                          {:input "test/t6/from_scala/guide.clj"
                           :title "from-scala"
                           :sub-title "A Scala interop library for Clojure"
                           :author "Tobias Kortkamp"
                           :email  "tobias.kortkamp@gmail.com"}}}
  :dependencies [[cats "0.1.0"
                  :exclusions [com.keminglabs/cljx
                               org.clojure/clojurescript]]
                 [potemkin "0.3.8"]]
  :aliases {"test-all" ["do" "midje," "with-profile" "+scala2.10" "midje"]})
