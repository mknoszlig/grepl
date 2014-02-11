(ns grepl.parser
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [grepl.lexer]))

;; A proper top-down parser
;;
;; The grammar includes whitespaces, comments and a generic line type, so that
;; we can reconstruct the source file even if we do not understand all of
;; it. It is assumed that we understand the major structures.

;; file
;;   line
;;   whitespace
;;   comment
;;   module
;;     class
;;       member
;;       method
;;     struct
;;       member
;;     enum
;;       definition
;;     interface
;;       method
;;     exception
;;       member

;;(defrecord Entity [type name content])

(defmulti parse-module (fn [[token]] (:type token)))
(defmulti parse-definition (fn [[token]] (:value token)))

(defn first-token-type? [tokens expected-type]
  (= (:type (first tokens)) expected-type))

(defn first-token-value? [tokens expected-value]
  (= (:value (first tokens)) expected-value))

(defn parse-until [tokens expected-type filter-ws]
  (if filter-ws
    [(filter
      #(not (#{:newline :ws :comment} (:type %)))
      (take-while #(not (expected-type (:type %))) tokens))
     (drop-while #(not (expected-type (:type %))) tokens)]
    [(take-while #(not (expected-type (:type %))) tokens)
     (drop-while #(not (expected-type (:type %))) tokens)]
    ))

(defn parse-line [tokens]
  (let [[line tokens] (parse-until tokens #{:newline :rbrace} false)]
    (if (first-token-type? tokens :newline)
      [{:type :line :content line} (next tokens)]
      [{:type :line :content line} tokens]
      )))

(defn parse-block [parse-fn tokens]
  ;; parse the header line, ignore ws, newline and comment
  ;; parse the block, recursively call parsing functions
  (let [[header tokens] (parse-until tokens #{:lbrace} true)]
    (assert (first-token-type? tokens :lbrace))
    (loop [content []
           tokens (next tokens)]
      (if (and tokens
               (first-token-type? tokens :rbrace))
        (let [[skip tokens] (parse-until (next tokens) #{:semicolon} true)]
          (assert (= (count skip) 0) (str "Expected 0 but found " (count skip) " tokens"))
          [header content (next tokens)])
        (let [[entry tokens] (parse-fn tokens)]
          (recur (conj content entry) tokens))
        ))))

(defn parse-exception [tokens]
  (parse-line tokens))

(defn parse-interface [tokens]
  (parse-line tokens))

(defmethod parse-module :newline [[token & r]] [token r])
(defmethod parse-module :ws [[token & r]] [token r])
(defmethod parse-module :comment [[token & r]] [token r])
(defmethod parse-module :semicolon [[token & r]] [token r])
(defmethod parse-module :identifier [tokens] (parse-definition tokens))
(defmethod parse-module :default [[token]]
  (when token
    (throw (Exception. (str "unexpected token type '" (:type token) "'")))))

(defmethod parse-definition "module" [tokens]
  (let [[header block tokens] (parse-block parse-module tokens)]
    ;; better error handling
    (assert (= (:type (first header) :identifier)))
    (assert (= (:value (first header) "module")))
    (assert (= (:type (second header) :identifier)))
    (assert (= (count header) 2))
    [{:type :module :name (:value (second header)) :content block} tokens]))

(defmethod parse-definition "exception" [tokens]
  (let [[header block tokens] (parse-block parse-exception tokens)]
    ;; better error handling
    (assert (= (:type (first header) :identifier)))
    (assert (= (:value (first header) "exception")))
    (assert (= (:type (second header) :identifier)))
    ;;(assert (= (count header) 2))
    [{:type :exception :name (:value (second header)) :content block} tokens]))

(defmethod parse-definition "interface" [tokens]
  (let [[header block tokens] (parse-block parse-interface tokens)]
    ;; better error handling
    (assert (= (:type (first header) :identifier)))
    (assert (= (:value (first header) "interface")))
    (assert (= (:type (second header) :identifier)))
    ;;(assert (= (count header) 2))
    [{:type :interface :name (:value (second header)) :content block} tokens]))

(defmethod parse-definition :default [tokens] (parse-line tokens))

(defn parse-seq
  [tokens]
  (lazy-seq
   (when-let [tokens (seq tokens)]
     (let [[entry tokens] (parse-module tokens)]
       (cons entry (parse-seq tokens))))))

(defn parse-file
  [url tokens]
  {:type :file :name url :content (parse-seq tokens)})

(defn parse
  "Parse the resource given by url"
  [url]
  (parse-file url (grepl.lexer/lex (slurp url))))

(comment
  (parse "https://raw2.github.com/ome/zeroc-ice/master/cpp/demo/book/simple_filesystem/Filesystem.ice")
  (def example (slurp "https://raw2.github.com/ome/zeroc-ice/master/cpp/demo/book/simple_filesystem/Filesystem.ice"))
  (parse-file "" (grepl.lexer/lex example))

  (parse-file "" (grepl.lexer/lex "module a {module b {module c {};};};"))
  (parse-file "" (grepl.lexer/lex "module a {class b {};};"))
  (parse-file "" (grepl.lexer/lex "module a {interface b {}; interface c {};};"))
  )
