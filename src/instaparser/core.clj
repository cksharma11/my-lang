(ns instaparser.core
 (:require [instaparse.core :refer :all]))

(def global-object (atom {}))

(defn choose-operator [operator]
 (case (first operator)
	:add '+
	:sub '-
	:div '/
	:mul '*))

(defn save [name value]
 (swap! global-object assoc name value))

(defn declare-function [fn-name args body]
	(save fn-name (eval (map read-string (list "fn" args body)))))

(defn transform-tree [tree]
 (let [node (first tree) s (second tree) terms (rest tree) length-expr (count terms)]
	(case node
	 :number (read-string s)
	 :identifier (symbol s)
	 :operator (choose-operator s)
	 :args (str (mapv (comp symbol second) terms))
	 :val (save (transform-tree s) (transform-tree (last tree)))
	 :function (declare-function (second s) (transform-tree (second terms)) (str (transform-tree (last terms))))
	 :expr (if (= length-expr 1)
					(transform-tree (first terms))
					(list (transform-tree (second terms))
								(transform-tree (first terms))
								(transform-tree (last terms)))))))

(def grammer-rules (parser (clojure.java.io/resource "grammer.bnf")))

(defn parse-my-lang [input]
  (->> (grammer-rules input) second transform-tree))

(defn repl []
 (let [input-string (read-line)]
	(parse-my-lang input-string)))

(defn -main []
 (loop []
	(println (repl))
	(recur)))