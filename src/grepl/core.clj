(ns grepl.core
  :require [clojure.string :as string])

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

(def scanner #"(?m)\s+|([a-zA-Z_]\w*)|\d+|(\"([^\"]|[\\].)*\")|(//.*)|(/\*.*\*/)|[\{\}\[\]\(\);:<>#.*]")

(defn trim-bol [token]
  (if (.contains token "\n")
    (.replaceAll token "[ \t]+" "")
    token))

(defn walk
  ([s] (walk s []))
  ([[h & t] acc]
     (cond (= "{" h) (let [[step new-t] (walk t [])]
                       (walk new-t (conj acc step)))
           (= "}" h) [acc t]
           (nil? h) acc
           :else (walk t (conj acc h)))))

(comment
  (map trim-bol (map first (re-seq scanner s)))
  (walk (map trim-bol (map first (re-seq scanner example-source))))
  )

(defmulti parse-tokens (fn [tokens] (first tokens)))

(defn proper-token? [token]
  (or
   (not (string? token))
   (not (re-matches #"(?m)(\s+|//.*|/\*.*)" token))))

(defn parse-definition [def-name tokens]
  (let [proper-tokens (filter proper-token? tokens)
        [_ name & proper-tokens] proper-tokens
        [_ base contents semi] (if (= (first proper-tokens) "extends")
                                proper-tokens
                                (cons nil (cons nil proper-tokens)))
        ]
    (assert (= semi ";") (str "Ice requires ';' at end of " def-name " definition, found '" semi "'"))
    (cons {:type def-name
           :name name
           :base base
           :contents (parse-tokens contents)}
          (parse-tokens (rest (drop-while #(not= ";" %) tokens))))))

(defmethod parse-tokens "module" [tokens] (parse-definition :module tokens))
(defmethod parse-tokens "interface" [tokens] (parse-definition :interface tokens))
(defmethod parse-tokens "exception" [tokens] (parse-definition :exception tokens))
(defmethod parse-tokens "struct" [tokens] (parse-definition :struct tokens))

(defmethod parse-tokens :default [[t & tokens]]
  (if t
    (cons t (parse-tokens tokens))
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
  (print (str indent "{"))
  (emit-all (:contents entry) (str indent "    "))
  (println)
  (print (str indent "};"))
  )

(defmethod emit :module [entry indent] (emit-definition entry indent))
(defmethod emit :interface [entry indent] (emit-definition entry indent))
(defmethod emit :exception [entry indent] (emit-definition entry indent))
(defmethod emit :struct [entry indent] (emit-definition entry indent))
(defmethod emit nil [entry indent]
  (if (.endsWith entry "\n")
    (print (str entry indent))
    (print entry)))

(comment
  (emit-all (parse-tokens (walk (map trim-bol (map first (re-seq scanner example-source))))))
  (print example-source)
  )
