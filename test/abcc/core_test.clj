(ns abcc.core-test
  (:require [clojure.test :refer :all]
            [abcc.core :refer :all]))

(deftest test-read-torrent
  (testing "it reads and parses a torrent file"
    (is (not (= 0 (count
                   (read-torrent "test/ubuntu-14.04-desktop-i386.iso.torrent"
                                ))))
        "non zero file opened")

    (is false
        "reads the announce attribute of a torrent file")))

(deftest test-read-bencode
         (testing "it reads bencoded numbers"
                  (is 123 (read-bencode "i123e"))
                  (is -3 (read-bencode "i-3e"))
         )
         (testing "it reads bencoded strings"
                  (is "super fuzzy" (read-bencode "11:super fuzzy"))
         ))
