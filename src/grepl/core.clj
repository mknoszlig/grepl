(ns grepl.core)

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
