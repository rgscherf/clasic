(ns clasic.lexer
  (:require [instaparse.core :as insta]))

(defn clasic-grammar
  []
  "<PROGRAM> = EXPR+
  EXPR = WHITESPACE? (SYMBOL | NUMBER | STRING | LET | FOR | CALL | CTX)
  CTX = <'ctx'> WHITESPACE? <'{'> WHITESPACE? EXPR* WHITESPACE? <'}'>
  FOR = WHITESPACE? <'for '> SYMBOL <' in '> EXPR <' {'> EXPR WHITESPACE-CR? <'}'>
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
  (insta/parser (clasic-grammar)))

(def clasic-lexer (lexer))
