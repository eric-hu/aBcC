(ns abcc.core-test
  (:require [clojure.test :refer :all]
            [abcc.core :refer :all]))

(deftest parsing-torrent-file
  (testing "it reads and parses the torrent test file"
    (is (not (= 0 (count
                   (read-torrent "test/ubuntu-14.04-desktop-i386.iso.torrent"
                                ))))
        "non zero file opened")
