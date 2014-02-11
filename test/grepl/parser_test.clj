(ns grepl.parser-test
  (:require [clojure.test :refer :all]
            [grepl.parser :refer :all]))

(deftest test-parse-file
  (are [s m] (= (:content (parse-file "" (grepl.lexer/lex s))) m)
       "" '()
       "a" '({:type :line, :content ({:type :identifier, :value "a"})})
       "module a {};" '({:type :module, :name "a", :content []})
       "module a {interface b {};};" '({:type :module, :name "a", :content [{:type :interface, :name "b", :content []}]})
       "module a {exception b {};};" '({:type :module, :name "a", :content [{:type :exception, :name "b", :content []}]})
       ))

(deftest test-parse-until
  (let [tokens (grepl.lexer/lex "module a {interface b {}; exception c {};};")]
    (is (= (parse-until tokens #{:lbrace} false)
           [(take 4 tokens) (drop 4 tokens)]))
    (is (= (parse-until tokens #{:lbrace} true)
           [[(nth tokens 0) (nth tokens 2)] (drop 4 tokens)]))
    ))
