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

(defn ^:internal build-sandbox [& [init]]
  (let [sandbox (js/Object.)]
    (doseq [prop sandbox-properties]
      ;; no need to check .hasOwnProperty
      (aset sandbox prop (aget js/global prop)))
    (doto sandbox
      (aset "require" js/require)
      ;; global aliases
      (aset "global" sandbox)
      (aset "GLOBAL" sandbox)
      (aset "root" sandbox)
      (aset "this" sandbox)
      (aset "_" sandbox))
    (doseq [prop (.keys js/Object init)]
      (when (.hasOwnProperty init prop)
        (aset sandbox prop (aget init prop))))
    sandbox))

(defn ^:internal read-line [data]
  (let [i (.indexOf data "\n")]
    (when (> i 0)
      (.slice data 0 (+ 1 i)))))

(defn ^:internal repl-data-fn
  "Returns a nodejs Stream 'data' event handler which recieves and parses JSON,
  evals js code, and handles the result using the provided result and error
  callback functions."
  [init result-fn error-fn]
  (let [context (.createContext vm (build-sandbox init))
        buffer (atom "")]
    (fn [data]
      (swap! buffer str data)
      (when-let [line (read-line @buffer)]
        (swap! buffer #(.slice % (count line)))
        (try* (let [json (.parse js/JSON line)
                    file (aget json "file")
                    code (aget json "code")]
                (if (and (string? code) (string? file))
                  (try* (result-fn (.runInContext vm code context file))
                        (catch e (error-fn e)))
                  (error-fn (cljs-format-error))))
              (catch e (error-fn (cljs-parse-error))))))))

(defn ^:internal encode [data]
  (.stringify js/JSON (clj->js data)))

(defn ^:internal decode [data]
  (js->clj (.parse js/JSON data)))

;; TODO: handle 'error' and 'end' events
(defn ^:internal init-repl [init stream]
  (assert stream (str "expected a stream!"))
  (let [write-fn  #(.write stream (str % "\n"))
        result-fn #(write-fn (encode {:result %}))
        error-fn  #(let [name    (str (.-name %))
                         message (str (.-message %))
                         stack   (str (.-stack %))]
                     (write-fn (encode {:error {:name name :message message :stack stack}})))]
    (doto stream
      (.setEncoding "utf-8")
      (.setKeepAlive true)
      (.on "data" (repl-data-fn init result-fn error-fn))
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
  [& [port host init]]
  (let [server (.createServer net (partial init-repl init))
        host (or host "localhost")
        port (or port 0)]
    (.listen server port host (partial repl-greeting server))
    server))
