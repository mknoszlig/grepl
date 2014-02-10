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

(defmethod parse-module :newline [[token & r]] [token r])
(defmethod parse-module :ws [[token & r]] [token r])
(defmethod parse-module :comment [[token & r]] [token r])

(defmethod parse-module :identifier [tokens]
  (let [line (take-while #(not= (:type %) :newline) tokens)
        r (drop-while #(not= (:type %) :newline) tokens)]
    [{:type :line :content line} r]))

(defmethod parse-module :default [[token]]
  (throw (Exception. (str "unexpected token type " (:type token)))))

(defn parse-seq
  [tokens]
  (lazy-seq
   (when (seq tokens)
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
  (parse-file "" (grepl.lexer/lex " a"))
  )
