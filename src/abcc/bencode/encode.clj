(ns abcc.bencode.encode
  (:gen-class))

(defn- encode-string [source]
  (str (.length source) \: source))

(defn- encode-number [source]
  (str \i source \e))

(declare to-bencoded-string)

(defn- encode-map [source]
  (let [contents (reduce-kv #(str %1
                                  (to-bencoded-string %2)
                                  (to-bencoded-string %3))
                            ""
                            source)]
    (str "d" contents "e")))

(defn to-bencoded-string [source]
  (condp = (class source)
   java.lang.String (encode-string source)
   java.lang.Long (encode-number source)
   clojure.lang.PersistentArrayMap (encode-map source)))
