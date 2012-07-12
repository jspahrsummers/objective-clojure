(ns objclj.repl
  (:gen-class)
  (:use clojure.stacktrace)
  (:use [objclj.codegen :only [codegen]])
  (:use [objclj.reader :only [parse]]))

(defn rep-line
  "Reads, evaluates, and prints a single line from stdin. Returns whether the REPL should continue."
  []
  (let [input (read-line)]
    (if (= input nil)
      ; User sent EOF/EOT, so exit gracefully
      (do
        (println)
        false)

      (try
        (let [ast (parse input)]
          (println ast)

          ; TODO: generate code for main()
          ; TODO: invoke Clang
          (println (codegen ast))
          true)

        ; Log any exceptions thrown during parsing or code generation and continue
        (catch Exception ex
          (print-cause-trace ex)
          true)))))

(defn repl
  "Main REPL loop."
  []
  (print "=> ")
  (flush)

  (if (rep-line) (recur)))

(defn -main [& args]
  (repl))
