(ns objclj.repl
  (:gen-class)
  (:use clojure.stacktrace)
  (:use [objclj.codegen :only [codegen]])
  (:use [objclj.reader :only [parse]]))

(defn -main [& args]
  (loop []
    (print "=> ")
    (flush)

    ;; (recur) can't appear within (try ...), so we have the latter return a boolean value indicating whether we should loop
    (if
      (try
        ; TODO: handle end-of-line, which should terminate the REPL
        (let [input (read-line)]
          (if (= input nil)
            (do
              (println)
              false)
            (let [ast (parse input)]
              (println ast)
              (flush)
              (println (codegen ast))
              true)))

        (catch Exception ex
          (print-cause-trace ex)
          true))

      (recur))))
