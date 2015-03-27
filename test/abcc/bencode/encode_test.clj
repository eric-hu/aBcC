(ns abcc.bencode.encode-test
  (:require [clojure.test :refer :all]
            [abcc.bencode.encode :refer :all]))

(deftest test-to-bencoded-string
  (testing
    "it returns an encoded string when passed a data structure"
    (is (= "11:super fuzzy" (to-bencoded-string "super fuzzy"))
        "Clojure strings are encoded to bencode strings")
    (is (= "i123e" (to-bencoded-string 123))
        "Clojure integers are encoded to bencode integers")
    (is (= "d1:ai2e1:bi3ee" (to-bencoded-string {"a" 2, "b" 3}))
        "Clojure hashmaps are encoded to bencode dictionaries")
    (is (= "le" (to-bencoded-string []))
        "Empty Clojure vectors are encoded to bencode lists")))
