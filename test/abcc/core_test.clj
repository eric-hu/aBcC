(ns abcc.core-test
  (:require [clojure.test :refer :all]
            [abcc.core :refer :all]))

(deftest test-read-torrent
  (testing "it reads and parses a torrent file"
    (is (not (= 0 (count
                    (read-torrent "test/ubuntu-14.04-desktop-i386.iso.torrent"
                                  ))))
        "non zero file opened")

    ; PENDING: clojure test doesn't have a pending :(
    ;(is false
    ;"reads the announce attribute of a torrent file")
    ))

(deftest test-read-bencode
  (testing
    "it reads a properly bencoded number"
    (is (= [123 ""] (read-bencode "i123e")))
    (is (= [-3 ""] (read-bencode "i-3e")))
    )

  (testing "it reads bencoded strings"
    ;(is (= "digit" (read-bencode "5:digit")))
    ;(is (= "super fuzzy" (read-bencode "11:super fuzzy")))
    ))

(deftest test-read-bencoded-integer
  (testing
    "it returns the number when passed a properly terminated number-substring"
    (is (= [123 "yolokitten"] (read-bencoded-integer "123eyolokitten")))
    (is (= [-3 ""] (read-bencoded-integer "-3e")))
    (is (= [0 ""] (read-bencoded-integer "0e")))
    )

  (testing "it raises an exception when passed an improperly terminated number"
    (is (thrown? Exception (read-bencoded-integer "123")))
    (is (thrown? Exception (read-bencoded-integer "123n")))
    (is (thrown? Exception (read-bencoded-integer "0n")))
    (is (thrown? Exception (read-bencoded-integer "0n0")))
    (is (thrown? Exception (read-bencoded-integer "1ne0")))
    )

  (testing "it raises an exception when passed an improperly terminated number"
    (is (thrown? Exception (read-bencoded-integer "123")))
    (is (thrown? Exception (read-bencoded-integer "123n")))
    (is (thrown? Exception (read-bencoded-integer "0n")))
    )

    (testing "it raises an exception when passed an invalid digit combination"
      (is (thrown? Exception (read-bencoded-integer "01e"))
          "the bittorrent spec prohibits zero-prefixed nonzero numbers")
      (is (thrown? Exception (read-bencoded-integer "001e"))
          "the bittorrent spec prohibits zero-prefixed nonzero numbers")
    )

    (testing "it raises an exception when passed negative zero"
      (is (thrown? Exception (read-bencoded-integer "-0e"))
          "the bittorrent spec prohibits negative zero")
      (is (thrown? Exception (read-bencoded-integer "-00e"))
          "the bittorrent spec prohibits negative zero")
      (is (thrown? Exception (read-bencoded-integer "-01e"))
          "the bittorrent spec prohibits negative zero-prefixed numbers")
      )

  (testing "it returns the rest of the string"
    (is (= [11 "5:hello"] (read-bencoded-integer "11e5:hello")))
    )
  )
