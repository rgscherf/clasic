(ns drty.lexer
  (:require [instaparse.core :as insta]))

;; still need:
;; - list comps/iteration?
;; - conditionals?
;;  - cond-only would be nice. if odd # of forms, final is the else.

(defn drty-grammar
  []
  "<PROGRAM> = EXPR+
  <EXPR> = WHITESPACE? (SYMBOL | NUMBER | STRING | LET | LISTCOMP | CALL | CTX)
  CTX = <'ctx'> WHITESPACE? <'{'> WHITESPACE? EXPR* WHITESPACE? <'}'>
  LISTCOMP = <'['> EXPR WHITESPACE <'|'> WHITESPACE EXPR <','>? WHITESPACE? EXPR? <']'>
  LET = WHITESPACE? <'let '> SYMBOL <' = '> EXPR
  CALL = EXPR <'('> EXPR* <')'>
  NUMBER = #'[0-9]+'
  SYMBOL = #'[a-zA-Z]+'
  STRING = #'\\'.*\\'' 
  <WHITESPACE> = <#'\\s+'>
  <WHITESPACE-CR> = <WHITESPACE> | <'\\n'>
  ")

(defn- lexer
  []
  (insta/parser (drty-grammar)))

(def drty-lexer (lexer))
