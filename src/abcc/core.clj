(ns abcc.core
  (:gen-class)
  (:require [clojure.pprint :as pp]
            [clj-http.client :as client]
            [abcc.bencode.decode :as decode]
            [abcc.bencode.encode :as encode]))

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
    (pp/pprint (client/get announce-url)))
  (println "Hello, World!"))

(defn announce-info-map
  [torrent-map]
  (let [info (:info torrent-map)
        encoded-info (encode/to-bencoded-string info)
        sha-encoded-info (apply str (.digest
                     (java.security.MessageDigest/getInstance "sha1")
                     (.getBytes encoded-info)))]
    sha-encoded-info))
