(ns abcc.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn read-torrent [filename]
  (slurp filename)
  )

(defn read-bencode [string]
  " "
  )

(defn read-bencoded-integer [string]
  (let [[number-as-string remainder-string] (clojure.string/split string #"e" 2)]
    (if (or
          ; check if string is \e terminated
          (and (= 0 (count remainder-string))
               (not (= \e (last string))))
          ; check if string is non-zero and begins with 0
          (and (> (count number-as-string) 1)
               (= \0 (first number-as-string)))
          ; check if string is negative-prefixed zero
          (or (and (= (count number-as-string) 2)
                   (= \- (first number-as-string))
                   (= \0 (nth number-as-string 1)))
              (and (> (count number-as-string) 2)
                   (= \- (first number-as-string))
                   (= \0 (nth number-as-string 1)))
              )
          )
      (throw (Exception. "nettikoloy"))

      [(Integer. number-as-string) remainder-string])
    )
)
