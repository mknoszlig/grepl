(ns grepl.core)

(def str "module Demo {
    interface Printer {
        void printString(string s);
    };
};")

(defn detect-module [str]
  (let [re #"(?s)module\s+(\S+)\s*\{(.*)$"]
    (re-find re str)))


(defn walk [[h & t] acc]
  (cond (= \{ h) (let [[step new-t] (walk t [])]
                   (walk new-t (conj acc step)))
        (= \} h) [acc t]
        (nil? h) acc
        :else (walk t (conj acc h))))
