(ns instaparser.core
 (:require [instaparse.core :as insta]))

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

(defn transform-tree [tree]
 (let [node (first tree) s (second tree) terms (rest tree) length-expr (count terms)]
	(case node
	 :number (read-string s)
	 :operator (choose-operator s)
	 :expr (if (= length-expr 1)
					(transform-tree (first terms))
					(list (transform-tree (second terms))
								(transform-tree (first terms))
								(transform-tree (last terms)))))))

(def parser
 (insta/parser
	"expr =  number | <'('> number space operator space number space <')'> | <'('> expr space operator space expr <')'>
	operator = add | sub | div | mul
	add = <'+'>
	sub = <'-'>
	div = <'/'>
	mul = <'*'>
	number = #'[0-9]+'
	<space> = <#' '*>"))

(defn parse [input]
 (->> (parser input) transform-tree))