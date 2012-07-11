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
        ; TODO: handle end-of-line exceptions from (read), which should terminate the REPL
        (println (parse (read-line)))
        ;(println (codegen (read)))
        true

        (catch Exception ex
          (e)
          true))

      (recur))))
