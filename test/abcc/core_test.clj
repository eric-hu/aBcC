(ns abcc.core-test
  (:require [clojure.test :refer :all]
            [abcc.core :refer :all]
            [abcc.bencode.decode :as decode]))

(deftest test-announce-info-map
  (testing
    "Given the a torrent file map, it bencodes it and takes the SHA-1"
    (is (= "p%01%2f%10M%e9%27%25%2f%ce%7cp%23%142%d0%ba%bbb%96"
           (announce-info-map (first (decode/read-torrent
                    "test/marriageofheaven00blak_archive.torrent"
                    "latin1")))))))
