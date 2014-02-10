(ns grepl.lexer
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))

;; This is very simple, using regex for reading a string.  The regex patterns
;; are supposed to cover all chars in the string, whitespaces and comments are
;; picked up as well.

;; Those regexes have names
(def pattern-strings {:newline "\n"
                      :ws "\\s+"
                      :identifier "([a-zA-Z_]\\w*)"
                      :integer "\\d+"
                      :string "(\\\"([^\\\"]|(\\.))*\\\")"
                      :comment "(//.*)|(/\\*.*\\*/)"
                      :operator "[\\[\\];:<>#.*]"
                      :lbrace "\\{"
                      :rbrace "\\}"
                      :lparen "\\("
                      :rparen "\\)"
                      })

;; we use the map of multiline patterns
(def patterns (apply hash-map
                     (mapcat (fn [[k v]]
                               [k (re-pattern (str "(?m)" v))])
                             pattern-strings)))

;; the actual scanner is a huge | of all patterns
(def scanner (->> pattern-strings
                  vals
                  ;(map #(str "(" % ")"))
                  (string/join "|")
                  (str "(?m)")
                  re-pattern))

(defn scan
  "Create a lazy seq of matches"
  [s]
  (map first (re-seq scanner s)))

;; use a distinct token type for a little premature optimization
(defrecord Token [type value])

(defn create-token [type value]
  {:type type :value value}
  ;; (Token. type value)
  )

(defn tokenize
  "Create a token type out of a match string, here we match a second time"
  [token]
  (first
   (filter identity
           (map (fn [[type re]]
                  (when (re-matches re token)
                    (create-token type token)))
                patterns))))

(defn lex
  "Create a lazy seq of token types"
  [s]
  (map tokenize (scan s)))

(comment
  (map tokenize (scan "a b
 c"))
  (lex "a \"b\" c /* comment */ 123")
  )

