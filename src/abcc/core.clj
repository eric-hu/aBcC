(ns abcc.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn read-torrent [filename]
  (slurp filename))

; Read an "e"-terminated integer out of string and return the number, parsed
; and converted to a Java integer and the remainder string.
;
; "33eadsf" => [33, "asdf"]
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
                   (= \0 (nth number-as-string 1)))))
      (throw (Exception. "nettikoloy"))

      [(Integer. number-as-string) remainder-string])))

; Read a bencoded string of the format:
;   "7:puppies"
; and returns a vector with the parsed string and the rest of the string
;   "7:puppies" => ["puppies" ""]
(defn read-bencoded-string [string]
  (let [[length-string remainder] (clojure.string/split string #":" 2)
       length (Integer. (apply str length-string))
       [parsed-string remainder] (split-at length remainder)
       ]
    [(apply str parsed-string) remainder]))

; read-bencoded-list
;
; Takes a string of bencode formatting and parses it in bencode type chunks.
; It stops when the first character is \e.
;
; Returns a vector with 2 values:
; 1. the parsed list, a vector itself
; 2. the remainder of the unparsed string

(defn- private-read-bencoded-list [string]
  )

(defn read-bencode-recur [string]
  (let [first-char (first string)]
    (condp = first-char
      ; Stopping condition: empty string (base recursion case)
      nil ""
      ; Integers
      \i (let [[parsed-int remaining-str]
               (read-bencoded-integer
                 ; cast char sequence to string
                 (apply str (rest string)))]
           (into [parsed-int]
                 (read-bencode-recur remaining-str)))
      ; Lists
      \l (let [[parsed-list remaining-stream]
               (private-read-bencoded-list (apply str (rest string)))]
               (into [parsed-list]
                     (read-bencode-recur remaining-stream)))

      ; else check if first character is a digit
      (if (Character/isDigit first-char)
        ; String
        (let [[parsed-str remaining-str]
              (read-bencoded-string string)
              remaining-str (apply str remaining-str)]
          (into [parsed-str]
                (read-bencode-recur remaining-str)))
        ; Unsupported bencode type
        (throw (Exception. (str
                             "Unrecognized bencode type.  First character: "
                             first-char)))))))

; Read a bencoded string and parse the results into a collection of
; clojure values
(defn read-bencode [string]
  (read-bencode-recur string))
