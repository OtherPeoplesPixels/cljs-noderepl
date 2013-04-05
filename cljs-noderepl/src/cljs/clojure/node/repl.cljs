(ns clojure.node.repl
  (:require [cljs.nodejs :as node]))

(def ^:private vm (node/require "vm"))
(def ^:private net (node/require "net"))

(defn ^:internal cljs-parse-error []
  (clj->js {:name "CljsParseError" :message "Can't parse input"}))

(defn ^:internal cljs-format-error []
  (clj->js {:name "CljsFormatError" :message "Input must be of the form '{:file \"<filename>\" :code \"<code>\"}'"}))

(def ^:internal sandbox-properties
  ["process" "console" "Buffer" "setTimeout" ;"require"
   "setInterval" "clearTimeout" "clearInterval" "DataView"
   "ArrayBuffer" "Int8Array" "Uint8Array" "Uint8ClampedArray"
   "Int16Array" "Uint16Array" "Int32Array" "Uint32Array"
   "Float32Array" "Float64Array"])

(defn ^:internal build-sandbox []
  (let [sandbox (js/Object.)]
    (doseq [prop sandbox-properties]
      ;; no need to check .hasOwnProperty
      (aset sandbox prop (aget js/global prop)))
    (doto sandbox
      (aset "goog" js/goog)
      (aset "cljs" js/cljs)
      (aset "require" js/require)
      (aset "global" sandbox) ;global aliases
      ;;(aset "GLOBAL" sandbox)
      ;;(aset "root" sandbox)
      ;;(aset "this" sandbox)
      ;;(aset "_" sandbox)
     )))

(defn ^:internal repl-data-fn
  "Returns a nodejs Stream 'data' event handler which recieves and parses JSON,
  evals js code, and handles the result using the provided result and error
  callback functions."
  [result-fn error-fn]
  (let [context (.createContext vm (build-sandbox))
        buffer (atom "")]
    (fn [data]
      (swap! buffer str data)
      (when-let [line (re-find #"[^\n]*\n" @buffer)]
        (swap! buffer #(.slice % (count line)))
        (try*
         (let [json (.parse js/JSON line)
               file (aget json "file")
               code (aget json "code")]
           (if (and (string? code) (string? file))
             (try*
              (result-fn (.runInContext vm code context file))
              ;;(result-fn (.runInThisContext vm code file))
              (catch e (error-fn e)))
             (error-fn (cljs-format-error))))
         (catch e (error-fn (cljs-parse-error))))))))

(defn ^:internal encode [data]
  (.stringify js/JSON (clj->js data)))

(defn ^:internal decode [data]
  (js->clj (.parse js/JSON data)))

;; TODO: handle 'error' and 'end' events
(defn ^:internal init-repl [stream]
  (assert stream (str "expected a stream!"))
  (let [write-fn  #(.write stream (str % "\n"))
        result-fn #(write-fn (encode {:result %}))
        error-fn  #(let [name    (str (.-name %))
                         message (str (.-message %))
                         stack   (str (.-stack %))]
                     (write-fn (encode {:error {:name name :message message :stack stack}})))]
    (doto stream
      (.setEncoding "utf-8")
      (.on "data" (repl-data-fn result-fn error-fn))
      ;;(.on "end" ...)
      ;;(.on "error" error-fn)
      ;;(.on "close" #(result-fn "Socket closed"))
      )))

(defn ^:internal repl-greeting
  "Prints a repl greeting with the server address."
  [server]
  (let [addr (.address server)
        host (.-address addr)
        port (.-port addr)]
    (.log js/console "node REPL started at %s:%s" host port)))

(defn connect
  "Connects to a REPL server from an nodejs application. After the connection is
  made, the REPL will evaluate forms in the context of the node vm that called
  this function. Returns the connected Stream object or throws an exception."
  [& [port host]]
  (let [server (.createServer net init-repl)]
    (.listen server (or port 0) (or host "localhost") (partial repl-greeting server))
    server))

;; DEBUG!!!
(set! *main-cli-fn* connect)
