(ns abcc.bencode.encode-test
  (:require [clojure.test :refer :all]
            [abcc.bencode.encode :refer :all]))

(deftest test-to-bencoded-string
  (testing
    "it returns an encoded string when passed a data structure"
    (is (= "11:super fuzzy" (to-bencoded-string "super fuzzy"))
        "Clojure strings are encoded to bencode strings")
    (is (= "i123e" (to-bencoded-string 123))
        "Clojure longs are encoded to bencode integers")
    (is (= "i123e" (to-bencoded-string (Integer. 123)))
        "Clojure integers are encoded to bencode integers")
    (is (= "d1:ai2e1:bi3ee" (to-bencoded-string {"a" 2, "b" 3}))
        "Clojure hashmaps are encoded to bencode dictionaries")
    (is (= "le" (to-bencoded-string []))
        "Empty Clojure vectors are encoded to bencode lists")
    (is (= "l2:abi166e3:abce" (to-bencoded-string ["ab", 166, "abc"]))
        "Clojure vectors are encoded into bencode lists")
    (is (= "d2:abi2e1:bi3ee" (to-bencoded-string {:ab 2, :b 3}))
        "Dictionaries convert keyword keys to strings, ordered by key")
    (is (= "ld3:fool3:bari2ee3:onei1eeli1e3:twoee"
           (to-bencoded-string [{"one" 1, "foo" ["bar", 2]}, [1, "two"]]))
        "Nested collections encode properly")
    (is (= "ld3:fool3:bard1:ali9000ei9001eeeeee"
           (to-bencoded-string [{"foo" ["bar" {:a [9000, 9001]}]}]))
        "Nested collections encode properly")))
