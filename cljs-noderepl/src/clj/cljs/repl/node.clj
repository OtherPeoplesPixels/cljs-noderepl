(ns cljs.repl.node
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [cljs.closure :as cljsc]
            [cljs.analyzer :as ana]
            [cljs.repl :as repl]
            [cheshire.core :refer [parse-string generate-string]]
            [cemerick.piggieback :as piggieback])
  (:import cljs.repl.IJavaScriptEnv
           java.net.Socket
           java.io.PipedReader
           java.io.PipedWriter))

(defn repl-socket
  "Create new repl connected socket."
  [{:keys [host port]}]
  (let [socket (Socket. host port)]
    {:socket socket
     :input  (io/reader (.getInputStream socket))
     :output (io/writer (.getOutputStream socket))}))

(defn js-eval [repl-env filename line code]
  (let [{:keys [input output]} repl-env]
    (.write output (str (generate-string {:file filename :line line :code code})
                        "\n"))
    (.flush output)
    (let [result-string (.readLine input)]
      (parse-string result-string true))))

(defn load-resource
  "Load a JS file from the classpath into the REPL repl-environment."
  [repl-env filename]
  (let [resource (io/resource filename)]
    (assert resource (str "Can't find " filename " in classpath"))
    (js-eval repl-env filename 1 (slurp resource))))

(defn node-setup [repl-env]
  (let [env (ana/empty-env)]
    (do (repl/load-file repl-env "cljs/core.cljs")
        (swap! (:loaded-libs repl-env) conj "cljs.core"))
    (repl/evaluate-form repl-env env "<cljs repl>" '(ns cljs.user))
    (repl/evaluate-form repl-env env "<cljs repl>" '(set! cljs.core/*print-fn* (.-print (js/require "util"))))))

(defn node-eval [repl-env filename line js]
  (let [result (js-eval repl-env filename line js)]
    (if-let [error (:error result)]
      {:status :exception :value (:stack error)}
      {:status :success :value (:result result)})))

(defn load-javascript [repl-env ns url]
  (let [missing (remove #(contains? @(:loaded-libs repl-env) %) ns)]
    (when (seq missing)
      (js-eval repl-env (.toString url) 1 (slurp url))
      (swap! (:loaded-libs repl-env) (partial apply conj) missing))))

(defn node-tear-down [repl-env]
  (let [socket (:socket repl-env)]
    (doto socket (.close))))

(defrecord NodeEnv []
  repl/IJavaScriptEnv
  (-setup [this]
    (node-setup this))
  (-evaluate [this filename line js]
    (node-eval this filename line js))
  (-load [this ns url]
    (load-javascript this ns url))
  (-tear-down [this]
    (node-tear-down this)))

(defn repl-env
  "Create a Node.js REPL environment.

  Options [default]:
    :host - The host to connect to [localhost]
    :port - The port to connect on [9000]"
  [{:keys [host port] :or {host "localhost" port 9000} :as opts}]
  (let [opts (assoc opts :host host :port port) ; delete if unecessary
        new-repl-env (merge (NodeEnv.)
                            {:optimizations :simple
                             :loaded-libs (atom #{})}
                            (repl-socket opts)
                            opts)]
    (load-resource new-repl-env "goog/base.js")
    (load-resource new-repl-env "goog/deps.js")
    new-repl-env))

(defn run-node-repl [opts]
  (repl/repl (repl-env opts)))

(defn nrepl-env [opts]
  (doto (repl-env opts) (node-setup)))

(defn run-node-nrepl [opts]
  (piggieback/cljs-repl :repl-env (nrepl-env opts)))
