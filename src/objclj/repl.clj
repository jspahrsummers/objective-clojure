(ns objclj.repl
  (:gen-class)
  (:use [objclj.codegen :only [codegen]]))

(defn -main [& args]
  (loop []
    (print "=> ")
    (flush)

    ;; (recur) can't appear within (try ...), so we have the latter return a boolean value indicating whether we should loop
    (if
      (try
        ; TODO: handle end-of-line exceptions from (read), which should terminate the REPL
        (println (codegen (read)))
        true

        (catch Exception e
          (println "*** Caught exception" e)
          true))

      (recur))))
