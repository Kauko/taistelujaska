(defproject taistelujaska "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [clj-http "3.4.1"]
                 [org.clojure/core.async "0.3.441"]
                 [astar-search "0.2"]]

  :source-paths ["src"]
  :test-paths ["test"]

  :repl-options
  {:init-ns taistelujaska.core
   :timeout 900000 ; 120s, needed for slow machines
   :port 61815}
  :main taistelujaska.core)
