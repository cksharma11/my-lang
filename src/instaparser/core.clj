(ns instaparser.core
  (:require [instaparse.core :refer :all]))

(defn choose-operator [operator]
  (case (first operator)
    :add 'clojure.core/+
    :sub 'clojure.core/-
    :div 'clojure.core//
    :mul 'clojure.core/*))

(def user-namespace (create-ns 'fun-lang.user))

(defn save [name value]
  (do
    (intern user-namespace name value)
    (in-ns 'instaparser.core)
    value))


(defn declare-function [fn-name args body]
  (let [fn-elements (map read-string (list "clojure.core/defn" fn-name args body))]
    (do
      (in-ns 'fun-lang.user)
      (eval fn-elements)
      (in-ns 'instaparser.core))))

(defn switch-ns-and-eval [fn-call]
  (do (in-ns 'fun-lang.user)
      (let [result (eval fn-call)]
        (in-ns 'instaparser.core)
        result)))

(defn transform-tree [tree]
  (let [node (first tree)
        s (second tree)
        terms (rest tree)
        length-expr (count terms)]
    (case node
      :number (read-string s)
      :identifier (symbol s)
      :operator (choose-operator s)
      :fn-call (conj (map transform-tree (rest terms)) (transform-tree s))
      :args (str (mapv (comp symbol second) terms))
      :val (save (transform-tree s) (transform-tree (last tree)))
      :function (declare-function
                  (second s)
                  (transform-tree (second terms))
                  (str (transform-tree (last terms))))
      :expr (if (= length-expr 1)
              (transform-tree (first terms))
              (list (transform-tree (second terms))
                    (transform-tree (first terms))
                    (transform-tree (last terms)))))))

(def grammer-rules
  (parser (clojure.java.io/resource "grammer.bnf")))

(defn parse-my-lang [input]
  (->> input
       grammer-rules
       second
       transform-tree
       switch-ns-and-eval))

(defn -main []
  (println "Welcome to Fun-Lang!")
  (loop []
    (print (str (ns-name user-namespace) ":=> "))
    (flush)
    (println (parse-my-lang (read-line)))
    (recur)))