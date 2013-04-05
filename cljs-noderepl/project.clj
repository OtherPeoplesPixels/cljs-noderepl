(defproject org.bodil/cljs-noderepl "0.1.7"
  :description "Node.js REPL environment for Clojurescript"
  :url "https://github.com/bodil/cljs-noderepl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojurescript "0.0-1576"]
                 [cheshire "5.0.1"]
                 [com.cemerick/piggieback "0.0.4"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :source-paths ["src/clj"]
  :cljsbuild {:builds [{:source-paths ["src/cljs"]
                        :compiler {:output-to "node_repl.js"
                                   :output-dir "out"
                                   :optimizations :simple
                                   :pretty-print true
                                   :target :nodejs}}]}
  ;;:hooks [leiningen.cljsbuild]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]})
