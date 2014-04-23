(ns abcc.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


(defn read-torrent [filename]
  (slurp filename)
  )

(defn read-bencode [string]
  1
  )
