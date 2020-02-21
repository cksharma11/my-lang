(ns instaparser.core
  (:require [instaparse.core :refer :all]))

(defn choose-operator [operator]
  (case (first operator)
    :add 'clojure.core/+
    :sub 'clojure.core/-
    :div 'clojure.core//
    :mul 'clojure.core/*
    :eq 'clojure.core/=
    :not-eq 'clojure.core/not=))

(def user-namespace (create-ns 'fun-lang.user))

(defn save [name value]
  (do
    (intern user-namespace name value)
    (in-ns 'instaparser.core)
    value))


(defn declare-function [fn-name args body]
  (let [fn-elements (map (comp read-string str)
                         (list "clojure.core/defn"
                               fn-name
                               args
                               body))]
    (do
      (in-ns 'fun-lang.user)
      (eval fn-elements)
      (in-ns 'instaparser.core))))

(defn switch-ns-and-eval [expr-list]
  (do
    (in-ns 'fun-lang.user)
    (let [result (eval expr-list)]
      (in-ns 'instaparser.core)
      result)))

(defn condition [[pred then-block else-block]]
  (list 'if pred then-block else-block))

(defmulti transform-tree first)

(defmethod transform-tree :number [tree]
  (read-string (second tree)))

(defmethod transform-tree :identifier [tree]
  (symbol (second tree)))

(defmethod transform-tree :operator [tree]
  (choose-operator (second tree)))

(defmethod transform-tree :fn-call [tree]
  (conj (map transform-tree (rest (rest tree)))
        (transform-tree (second tree))))

(defmethod transform-tree :args [tree]
  (mapv (comp symbol second) (rest tree)))

(defmethod transform-tree :val [tree]
  (save (transform-tree (second tree))
        (transform-tree (last tree))))

(defmethod transform-tree :condition [tree]
  (condition (map transform-tree (rest tree))))

(defmethod transform-tree :function [tree]
  (declare-function
    (transform-tree (second tree))
    (transform-tree (second (rest tree)))
    (transform-tree (last (rest tree)))))

(defmethod transform-tree :expr [tree]
  (let [terms (rest tree)]
    (if (= (count terms) 1)
      (transform-tree (first terms))
      (list (transform-tree (second terms))
            (transform-tree (first terms))
            (transform-tree (last terms))))))

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