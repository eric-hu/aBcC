(ns abcc.core
  (:gen-class)
  (:require [clojure.pprint :as pp]
            [clj-http.client :as client]
            [abcc.bencode.decode :as decode]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (let [torrent-info (first
                       (decode/read-torrent
                         "test/marriageofheaven00blak_archive.torrent"
                         "latin1"))
        announce-url (:announce torrent-info)]
    (pp/pprint announce-url)
    #_(pp/pprint (client/get announce-url)))
  (println "Hello, World!"))
