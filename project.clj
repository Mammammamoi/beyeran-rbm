(defproject beyeran-rbm "0.1.0-SNAPSHOT"
  :description "A Restricted Boltzman Machine in Clojure. This program should be used to
                pretrain neural network layers."
  :url "http://github.com/beyeran/beyeran-rbm.git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter "1.5.5"]]
  :main ^:skip-aot beyeran-rbm.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
