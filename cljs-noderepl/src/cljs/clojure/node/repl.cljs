(ns clojure.node.repl
  (:require [cljs.nodejs :as node]))

(def vm (node/require "vm"))
(def net (node/require "net"))

(def parse-error (clj->js {:name "CljParseError" :message "Can't parse input"}))

(defn stream-data-fn
  [result-fn error-fn]
  (fn [data]
    (let [buffer (atom "")]
      (swap! buffer str data)
      (loop [line (re-find #"[^\n]*\n" @buffer)]
        (when line
          (swap! buffer #(.slice % (count line)))
          (try*
           (let [{:keys [code file]} (.parse js/JSON data)]
             (if (and (string? code) (string? file))
               (try*
                (result-fn (vm/runInThisContext code file))
                (catch e (error-fn e))))
             (error-fn "Input must be of the form '{:file \"<filename>\" :code \"<code>\"}'"))
           (catch e (error-fn parse-error))))))))

;; TODO: handle 'error' and 'end' events
(defn init-repl [stream]
  (let [w #(.write stream (str % "\n"))]
    (doto stream
      (.setEncoding "utf-8")
      (.on "data"
           (stream-data-fn
            #(.write stream (str "{\"result\":" (.stringify js/JSON %) "}\n"))
            #(.write stream (str "{\"error\":" (.stringify js/JSON %) "}\n")))))))

(def connection (atom nil))

(defn connect
  [& {:as opts}]
  (if-let [conn (.connect net (clj->js opts) init-repl)]
    (reset! connection conn)
    (throw (ex-info "Could not connect to socket!" opts))))