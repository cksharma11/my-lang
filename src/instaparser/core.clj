(ns instaparser.core
 (:require [instaparse.core :as insta]))

(def global-object (atom {}))

(def transform-options
 {:number read-string
	:add    (partial +)
	:sub    (partial -)
	:div    (partial /)
	:mul    (partial *)})

(defn choose-operator [operator]
 (case (first operator)
	:add '+
	:sub '-
	:div '/
	:mul '*))


(defmacro define-fn [args body]
 `(fn ~args ~body))

(defn declare-function [fn-name args body]
 (swap! global-object assoc fn-name (define-fn args body)))

(defn transform-tree [tree]
 (println tree)
 (let [node (first tree) s (second tree) terms (rest tree) length-expr (count terms)]
	(case node
	 :number (read-string s)
	 :operator (choose-operator s)
	 :args (mapv second terms)
	 :function (declare-function (second s) (transform-tree (second terms)) (transform-tree (last terms)))
	 :expr (if (= length-expr 1)
					(transform-tree (first terms))
					(list (transform-tree (second terms))
								(transform-tree (first terms))
								(transform-tree (last terms)))))))

(def parser
 (insta/parser
	"language = function | expr
	expr =  number | <'('> number space operator space number space <')'> | <'('> expr space operator space expr <')'>
	function = <'fun'> space+ identifier space+ args space+ <':'> space expr
	identifier = #'[a-z]+[0-9]*[a-z,A-Z]*'
	args = <'['> (identifier space+)* identifier <']'>
	operator = add | sub | div | mul
	add = <'+'>
	sub = <'-'>
	div = <'/'>
	mul = <'*'>
	number = #'[0-9]+'
	<space> = <#' '*>"))

(defn parse [input]
 (->> (parser input) second transform-tree))