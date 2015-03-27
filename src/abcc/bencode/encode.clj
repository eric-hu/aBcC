(ns abcc.bencode.encode
  (:gen-class))

(defmulti to-bencoded-string class)

(defmethod to-bencoded-string java.lang.String [source]
  (str (.length source) \: source))

(defmethod to-bencoded-string java.lang.Long [source]
  (str \i source \e))

(defmethod to-bencoded-string java.util.Map [source]
  (let [contents (->> source
                      seq
                      flatten
                      (map to-bencoded-string)
                      (apply str))]
    (str "d" contents "e")))

(defmethod to-bencoded-string java.util.List [source]
  (str "l"
      (apply str (map to-bencoded-string source))
      "e"))
