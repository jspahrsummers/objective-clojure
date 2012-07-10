(ns objclj.repl
  (:use objclj.codegen))

(defn -main [& args]
  (loop []
    (print "=> ")
    (flush)
    (if
      (try
        ; TODO: handle end-of-line exceptions from (read), which should terminate the REPL
        (println (eval (read)))
        true
        (catch Exception e
          (println "*** Caught exception" e)
          true))
      (recur))))
