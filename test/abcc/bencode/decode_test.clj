(ns abcc.bencode.decode-test
  (:require [clojure.test :refer :all]
            [abcc.bencode.decode :refer :all]))

(deftest test-read-torrent
  (testing "it reads and parses a torrent file"
    (is (thrown? Exception
                 (read-torrent "test/marriageofheaven00blak_archive.torrent"))
        "reading a file with the wrong encoding throws an exception")
    (is (not (=
              0
              (count
                (read-torrent "test/marriageofheaven00blak_archive.torrent"
                              "latin1"))))
        "it takes an argument specifying the file encoding type")

    ; PENDING: clojure test doesn't have a pending :(
    #_(is false
        "reads the announce attribute of a torrent file")))

(deftest test-read-bencode
  (testing "Numbers"
    (is (= [123] (read-bencode "i123e"))
        "it reads a properly bencoded positive number")
    (is (= [-3] (read-bencode "i-3e"))
        "it reads a properly bencoded negative number")
    (is (= [321 12 3 345] (read-bencode "i321ei12ei3ei345e"))
        "it reads multiple bencoded numbers"))

  (testing "Strings"
    (is (= ["digit"] (read-bencode "5:digit"))
       "it reads bencoded strings")
    (is (= ["super fuzzy"] (read-bencode "11:super fuzzy"))
        "it reads bencoded strings")
    (is (= ["yolokitten"] (read-bencode "10:yolokitten"))
        "it reads bencoded strings")
    (is (= ["yolokitten" "yolopuppy"]
         (read-bencode "10:yolokitten9:yolopuppy"))
        "it reads bencoded strings"))

  (testing "Lists"
    (is (= [["big daddy" "little daddy"]]
           (read-bencode "l9:big daddy12:little daddye"))
        "it reads simple bencoded lists into vectors")
    (is (= [["big daddy" "little daddy" 15] "pies"]
           (read-bencode "l9:big daddy12:little daddyi15ee4:pies"))
        "it reads a list with other bencode types"))

  (testing "Nested Lists"
    (is (= [["lolcat" ["paws"]] "bark"]
           (read-bencode "l6:lolcatl4:pawsee4:bark"))
        "it reads nested lists")
    (is (= [["deeply" ["nested" ["list"]]]]
           (read-bencode "l6:deeplyl6:nestedl4:listeee"))
        "it reads deeply nested lists")
    (is (= [["complex" {:nested "list"}]]
           (read-bencode "l7:complexd6:nested4:listee"))
           "it reads a list with dictionary"))

  (testing "Simple Dictionaries"
    (is (= [{:big "daddy"}] (read-bencode "d3:big5:daddye"))
            "dictionaries parse into maps with keywords for hash keys")
    (is (= [{:big "daddy"}] (read-bencode "d3:big5:daddye"))
            "dictionaries can have string hash-values"))

  (testing "Nested Dictionaries"
    (is (= [{:big {:freakin {:hash "map"}}}]
           (read-bencode "d3:bigd7:freakind4:hash3:mapeee"))
        "it reads dictionaries within dictionaries")
    (is (= [{:hash ["list"]}]
           (read-bencode "d4:hashl4:listee"))
        "it reads dictionaries containing lists"))

  (testing "Mixed types"
    (is (= [2 "fuzzy kittens"] (read-bencode "i2e13:fuzzy kittens"))
        "it reads mixed bencoded strings and integers")
    (is (= ["fuzzy kittens" 2] (read-bencode "13:fuzzy kittensi2e"))
        "it reads mixed bencoded strings and integers"))

  (testing "Errors"
    (is (thrown? Exception (read-bencode "z"))
        "it raises an exception when given an invalid bencode string")))

(deftest test-read-bencoded-string

  (testing
    "it returns the parsed string when passed a properly bencoded string"
    (is (= ["super fuzzy" '()] (read-bencoded-string "11:super fuzzy"))))

  (testing
    "it returns the parsed string when passed a properly bencoded character sequence"
    (is (= ["super" '()] (read-bencoded-string '(\5 \: \s \u \p \e \r))))))

(deftest test-read-bencoded-integer
  (testing
    "it returns the number when passed a properly terminated number-substring"
    (is (= [123 "yolokitten"] (read-bencoded-integer "123eyolokitten")))
    (is (= [-3 ""] (read-bencoded-integer "-3e")))
    (is (= [0 ""] (read-bencoded-integer "0e"))))

  (testing
    "it works with character sequences that aren't strings"
    (is (= [123 ""] (read-bencoded-integer '(\1 \2 \3 \e)))))

  (testing "it raises an exception when passed an improperly terminated number"
    (is (thrown? Exception (read-bencoded-integer "123")))
    (is (thrown? Exception (read-bencoded-integer "123n")))
    (is (thrown? Exception (read-bencoded-integer "0n")))
    (is (thrown? Exception (read-bencoded-integer "0n0")))
    (is (thrown? Exception (read-bencoded-integer "1ne0"))))

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
