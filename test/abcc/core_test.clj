(ns abcc.core-test
  (:require [clojure.test :refer :all]
            [abcc.core :refer :all]))

(deftest test-read-torrent
  (testing "it reads and parses a torrent file"
    (is (not (=
              0
              (count
                (read-torrent "test/ubuntu-14.04-desktop-i386.iso.torrent"))))
        "non zero file opened")

    ; PENDING: clojure test doesn't have a pending :(
    #_(is false
        "reads the announce attribute of a torrent file")))

(deftest test-read-bencode
  ; Numbers
  (testing
    "it reads a properly bencoded number"
    (is (= [123] (read-bencode "i123e")))
    (is (= [-3] (read-bencode "i-3e"))))

  (testing
    "it reads multiple bencoded numbers"
    (is (= [123 12] (read-bencode "i123ei12e")))
    (is (= [321 12 3 345] (read-bencode "i321ei12ei3ei345e"))))

  ; Strings
  (testing "it reads bencoded strings"
    (is (= ["digit"] (read-bencode "5:digit")))
    (is (= ["super fuzzy"] (read-bencode "11:super fuzzy")))
    (is (= ["yolokitten"] (read-bencode "10:yolokitten")))
    (is (=
         ["yolokitten" "yolopuppy"]
         (read-bencode "10:yolokitten9:yolopuppy"))))

  ; Lists
  (testing "it reads simple bencoded lists into vectors"
    (is (=
         [["big daddy" "little daddy"]]
         (read-bencode "l9:big daddy12:little daddye"))))
  (testing "it reads a list with other bencode types"
    (is (=
         [["big daddy" "little daddy"] "pies"]
         (read-bencode "l9:big daddy12:little daddye4:pies"))))

  ; Mixed: string-number
  (testing
    "it reads mixed bencoded strings and integers"
    (is (= [2 "fuzzy kittens"] (read-bencode "i2e13:fuzzy kittens")))
    (is (= ["fuzzy kittens" 2] (read-bencode "13:fuzzy kittensi2e"))))

  ; Errors
  (testing "it raises an exception when given an invalid bencode string"
    (is (thrown? Exception (read-bencode "z")))))

(deftest test-read-bencoded-integer
  (testing
    "it returns the number when passed a properly terminated number-substring"
    (is (= [123 "yolokitten"] (read-bencoded-integer "123eyolokitten")))
    (is (= [-3 ""] (read-bencoded-integer "-3e")))
    (is (= [0 ""] (read-bencoded-integer "0e"))))

  (testing "it raises an exception when passed an improperly terminated number"
    (is (thrown? Exception (read-bencoded-integer "123")))
    (is (thrown? Exception (read-bencoded-integer "123n")))
    (is (thrown? Exception (read-bencoded-integer "0n")))
    (is (thrown? Exception (read-bencoded-integer "0n0")))
    (is (thrown? Exception (read-bencoded-integer "1ne0"))))

  (testing "it raises an exception when passed an improperly terminated number"
    (is (thrown? Exception (read-bencoded-integer "123")))
    (is (thrown? Exception (read-bencoded-integer "123n")))
    (is (thrown? Exception (read-bencoded-integer "0n"))))

    (testing "it raises an exception when passed an invalid digit combination"
      (is (thrown? Exception (read-bencoded-integer "01e"))
          "the bittorrent spec prohibits zero-prefixed nonzero numbers")

      (is (thrown? Exception (read-bencoded-integer "001e"))
          "the bittorrent spec prohibits zero-prefixed nonzero numbers"))

    (testing "it raises an exception when passed negative zero"
      (is (thrown? Exception (read-bencoded-integer "-0e"))
          "the bittorrent spec prohibits negative zero")
      (is (thrown? Exception (read-bencoded-integer "-00e"))
          "the bittorrent spec prohibits negative zero")
      (is (thrown? Exception (read-bencoded-integer "-01e"))
          "the bittorrent spec prohibits negative zero-prefixed numbers"))

  (testing "it returns the rest of the string"
    (is (= [11 "5:hello"] (read-bencoded-integer "11e5:hello")))))

