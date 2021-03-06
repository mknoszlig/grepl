(ns grepl.core
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]))

(comment

(comment
  (require 'clojure.java.javadoc)
  (clojure.java.javadoc/add-local-javadoc "/usr/share/doc/openjdk-7-doc/api/")
  )

(def s "module Demo {
    interface Printer {
        void printString(string s);
    };
};

module Foo {
};")

(defn update-brackets [m c]
  (cond (= c \{) (update-in m [:curly] inc)
        (= c \}) (update-in m [:curly] dec)
        (= c \() (update-in m [:round] inc)
        (= c \)) (update-in m [:round] dec)
        (= c \[) (update-in m [:square] inc)
        (= c \]) (update-in m [:square] dec)
        :else m))

(def spacey? #{\space \newline})

(defn tokenize-level
  ([s]
     (tokenize-level s 0))
  ([s level]
     (loop [global-acc []
            {:keys [curly square round] :as bracket-level} {:curly 0
                                                            :square 0
                                                            :round 0}
            word-acc []
            [h & t] s]
       (cond (nil? h) (conj global-acc (apply str word-acc))
             (or (> curly level)
                 (not (spacey? h))) (recur global-acc
                                         (update-brackets bracket-level h)
                                         (conj word-acc h)
                                         t)
                 (spacey? h) (recur (conj global-acc (apply str word-acc))
                                    (update-brackets bracket-level h)
                                     []
                                     t)))))

(defmulti parse-keyword (fn [tokens level] (first tokens)))

(defmethod parse-keyword "module" [[_ name contents & more] level]
  (cons
   {:module name
    :contents (parse-keyword (tokenize-level contents (inc level)) (inc level))}
   (parse-keyword more level)))

(defmethod parse-keyword "interface" [[_ name contents & more] level]
  (cons
   {:interface name
    :contents (parse-keyword (tokenize-level contents (inc level)) (inc level))}
   (parse-keyword more level)))

(defmethod parse-keyword :default [[t & tokens] level]
  (if t
    (vec (cons t (parse-keyword tokens level)))
    []))


;; example usage:
;; (-> s (tokenize-level) (parse-keyword 0))

(def example-source (slurp "https://raw2.github.com/ome/zeroc-ice/master/cpp/demo/book/simple_filesystem/Filesystem.ice"))

;; We will need a proper top-down parser. We need to add functions (in
;; interfaces), enum values (in enums), and member variables in structs. These
;; are the items we want to modify later on.
;;
;; Since there can be comments and empty lines at all places, we need to add
;; those to our grammar. This is why we need token types.

(def pattern-strings {:ws "\\s+"
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
(def patterns (apply hash-map (mapcat (fn [[k v]] [k (re-pattern (str "(?m)" v))]) pattern-strings)))

;; named groups are not helpful here, the name is only usable within the regex
;;
;; (def scanner (->> pattern-strings
;;                   (map (fn [[k v]] (str "(?<" (name k) ">" v ")")))
;;                   (string/join "|")
;;                   (str "(?m)")
;;                   re-pattern))

(def scanner (->> pattern-strings
                  vals
                  ;(map #(str "(" % ")"))
                  (string/join "|")
                  (str "(?m)")
                  re-pattern))

(defrecord Token [type value])

(defn create-token [type value]
  {:type type :value value})

(defn tokenize [[token]]
  (first
   (filter identity
           (map (fn [[k v]]
                  (when (re-matches v token)
                    (create-token k token)))
                patterns))))

;; (re-matches scanner " ")

(comment
  (defmulti trim-bol :type)

  (defmethod trim-bol :ws [token]
    (if (.contains (:value token) "\n")
      (create-token :ws (.replaceAll (:value token) "[ \t]+" ""))
      token))

  (defmethod trim-bol :default [token] token)
  )

(defn trim-bol :ws [token]
  (if (and (= (:type token) :ws)
           (.contains (:value token) "\n"))
    (create-token :ws (.replaceAll (:value token) "[ \t]+" ""))
    token))

(defn pop-while [pred list]
  (let [last-element (peek list)]
    (if (and last-element
             (pred last-element))
      (recur pred (pop list))
      list)))

(defn walk
  ([s] (walk s []))
  ([[h & t] acc]
     (cond (#{:lbrace :lparen} (:type h)) (let [[step new-t] (walk t [h])]
                                            (walk new-t (conj acc step)))
           (#{:rbrace :rparen} (:type h)) [(conj acc h) t]
           (nil? h) acc
           :else (recur t (conj acc h)))))

(comment
  (map :value (map trim-bol (map tokenize (re-seq scanner s))))
  (walk (map trim-bol (map tokenize (re-seq scanner example-source))))
  )

(defmulti parse-module (fn [[token]] (:value token)))
(defmulti parse-function (fn [[token]] (:type token)))

(defn proper-token? [token]
  (not (#{:ws :comment} (:type token))))

(defn parse-definition [def-name parse-content tokens]
  (let [proper-tokens (filter proper-token? tokens)
        [_ name contents semi & proper-tokens] proper-tokens
        ]
    (assert (= (:value semi) ";") (str "Ice requires ';' at end of " def-name " definition, found '" semi "'"))
    (cons {:type def-name
           :name (:value name)
           :contents (parse-content contents)}
          (parse-module (rest (drop-while #(not= ";" (:value %)) tokens))))))

(defn parse-extendable-definition [def-name parse-content tokens]
  (let [proper-tokens (filter proper-token? tokens)
        [_ name & proper-tokens] proper-tokens
        [_ base contents semi] (if (= (:value (first proper-tokens)) "extends")
                                 proper-tokens
                                 (cons nil (cons nil proper-tokens)))
        ]
    (assert (= (:value semi) ";") (str "Ice requires ';' at end of " def-name " definition, found '" semi "'"))
    (cons {:type def-name
           :name (:value name)
           :base (:value base)
           :contents (parse-content contents)}
          (parse-module (rest (drop-while #(not= ";" (:value %)) tokens))))))

(defmethod parse-module "module" [tokens] (parse-definition :module parse-module tokens))
(defmethod parse-module "interface" [tokens] (parse-extendable-definition :interface parse-function tokens))
(defmethod parse-module "exception" [tokens] (parse-extendable-definition :exception parse-module tokens))
(defmethod parse-module "class" [tokens] (parse-extendable-definition :class parse-module tokens))
(defmethod parse-module "struct" [tokens] (parse-definition :struct parse-module tokens))
(defmethod parse-module "enum" [tokens] (parse-definition :enum parse-module tokens))

(defmethod parse-module :default [[t & tokens]]
  (if t
    (cons t (parse-module tokens))
    []))

(defmethod parse-function :identifier [tokens]
  (let [proper-tokens (filter proper-token? tokens)
        [name return-type & modifiers] (map :value (reverse (take-while :type proper-tokens)))
        [args & proper-tokens] (drop-while :type proper-tokens)
        args (->> args
                  rest
                  butlast
                  (map :value)
                  (partition-by #(= "," %))
                  (take-nth 2)
                  )
        [exceptions & proper-tokens] (if (-> proper-tokens first :value (= "throws"))
                                       (cons (->> proper-tokens
                                                  rest
                                                  (map :value)
                                                  (take-while #(not= % ";"))
                                                  (partition-by #(=  % ","))
                                                  (take-nth 2)
                                                  (mapcat identity))
                                             (->> proper-tokens
                                                  (drop-while #(-> % :value (not= ";")))))
                                       (cons nil proper-tokens))
        [semi] proper-tokens
        ]
    (assert (= (:value semi) ";") (str "Ice requires ';' at end of " name " definition, found '" semi "'"))
    (cons {:type :function
           :name name
           :return-type return-type
           :modifiers modifiers
           :args args
           :exceptions exceptions}
          (parse-function (rest (drop-while #(not= ";" (:value %)) tokens))))))

(defmethod parse-function :default [[t & tokens]]
  (if t
    (cons t (parse-function tokens))
    []))

(defmulti emit (fn [entry indent] (:type entry)))

(defn emit-all
  ([tokens] (emit-all tokens ""))
  ([tokens indent]
     (doseq [token tokens]
       (emit token indent))))

(defn emit-definition [entry indent]
  (println (str (name (:type entry)) " " (:name entry)
                (when (:base entry) (str " extends " (:base entry)))))
  (print indent)
  (emit-all (:contents entry) (str indent "    "))
  (println)
  (print ";")
  )

(defmethod emit :module [entry indent] (emit-definition entry indent))
(defmethod emit :interface [entry indent] (emit-definition entry indent))
(defmethod emit :exception [entry indent] (emit-definition entry indent))
(defmethod emit :class [entry indent] (emit-definition entry indent))
(defmethod emit :struct [entry indent] (emit-definition entry indent))
(defmethod emit :enum [entry indent] (emit-definition entry indent))
(defmethod emit :function [entry indent]
  (print indent)
  (print (apply str (interleave (:modifiers entry) (repeat " "))))
  (print (str (:return-type entry) " " (:name entry)))
  (print "(")
  (print (apply str (interpose "," (map string/join (:args entry)))))
  (print ")")
  (when (:exceptions entry)
    (print (apply str (cons " throws " (interpose ", " (:exceptions entry))))))
  (print ";")
  )
(defmethod emit :default [entry indent]
  (let [value (:value entry)]
    (if (.endsWith value "\n")
      (print (str value indent))
      (print value))))

(comment
  (emit-all (parse-module (walk (map trim-bol (map tokenize (re-seq scanner example-source))))))
  (print example-source)
  )
)
