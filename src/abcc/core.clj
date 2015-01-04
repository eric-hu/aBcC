(ns abcc.core
  (:gen-class))

; private-split-string-at-e
;
; Input: a string or character sequence
;   Example: "aedef"
;
; Output: a vector of two character sequences that add up to the input string,
; split at the first "e", with the splitting "e" dropped.
;   Example: [(\a) (\d \e \f)]
(defn- private-split-string-at-e [string]
  (clojure.string/split (apply str string) #"e" 2))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(defn read-torrent [filename]
  (slurp filename))

; private-string-ends-with-e
; helper function for integer parsing
(defn- private-string-ends-with-e
  [remainder-string string]
  (and (= 0 (count remainder-string))
           (not (= \e (last string)))))

; private-string-is-non-zero-and-begins-with-zero
; helper function for integer parsing
(defn- private-string-is-non-zero-and-begins-with-zero
  [number-as-string]
  (and (> (count number-as-string) 1)
         (= \0 (first number-as-string))))

; private-string-is-negative-prefixed-zero
; helper function for integer parsing
(defn- private-string-is-negative-prefixed-zero
  [number-as-string]
  (or (and (= (count number-as-string) 2)
             (= \- (first number-as-string))
             (= \0 (nth number-as-string 1)))
        (and (> (count number-as-string) 2)
             (= \- (first number-as-string))
             (= \0 (nth number-as-string 1)))))

; read-bencoded-integer
;
; Input: an integer in a character sequence in the format: "<integer>e" and
; possibly some other bencoded values following.
;   Example:
;   (\3 \3 \e \4 \: \a \d \s \f)
;
; Output: a vector of the Java integer and the remainder character sequence.
;   Example:
;   [33, (\4 \: \a \s \d \f)]
(defn read-bencoded-integer [string]
  (let [[number-as-string remainder-string] (private-split-string-at-e string)]
    (if (or
          (private-string-ends-with-e remainder-string string)
          (private-string-is-non-zero-and-begins-with-zero number-as-string)
          (private-string-is-negative-prefixed-zero number-as-string))

      (throw (Exception. "preconditions failed for integer bencode parsing"))

      [(Integer. number-as-string) remainder-string])))

; read-bencoded-string
;
; Input: a bencoded character sequence of the format "<length>:<string>"
;   Example:
;   '(\7 \: \p \u \p \p \i \e \s)
;
; Output: a vector with the parsed string and the rest of the character sequence
;   Example:
;   ["puppies" '()]
(defn read-bencoded-string [string]
  (let [string (apply str string)
        [length-string remainder] (clojure.string/split string #":" 2)
        length (Integer. length-string)
        [parsed-string remainder] (split-at length remainder)]
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
  (let [first-char (first string)
        rest-string (rest string)]
    (condp = first-char
      ; Stopping condition: first character is "e"
      \e [[] rest-string]

      (if (Character/isDigit first-char)
        ; Strings
        (let [[parsed-str remaining-str] (read-bencoded-string string)
              [partial-res remaining-string] (private-read-bencoded-list remaining-str)
              res-vector (into [parsed-str] partial-res)
              res [res-vector remaining-string]]
            res)
        ; Unsupported bencode type
        (throw (Exception. (str
                             "Unrecognized bencode-list type.  First character: "
                             first-char)))))))

; read-bencode-recur
;
; [string]
;
; Takes a string of bencode formatting and parses it one chunk at a time,
; recurring on the rest.  Stops when the first character of the string is nil.
;
; Returns the parsed values as a vector
(defn read-bencode-recur [string]
  (let [first-char (first string)]
    (condp = first-char
      ; Stopping condition: empty string (base recursion case)
      nil ""
      ; Integers
      \i (let [[parsed-int remaining-str] (read-bencoded-integer (rest string))]
           (into [parsed-int] (read-bencode-recur remaining-str)))
      ; Lists
      \l (let [[parsed-list remaining-stream]
               (private-read-bencoded-list (rest string))]
               (into [parsed-list]
                     (read-bencode-recur remaining-stream)))

      ; else check if first character is a digit
      (if (Character/isDigit first-char)
        ; String
        (let [[parsed-str remaining-str] (read-bencoded-string string)]
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
