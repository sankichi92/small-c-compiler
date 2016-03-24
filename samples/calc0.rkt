#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

(define-lex-abbrevs
  (digit   (char-range "0" "9")))

(define calc-lexer
  (lexer
   ((:+ digit)    lexeme)
   ((:or "+" "*") lexeme)
   (whitespace    (calc-lexer input-port))
   ((eof)         'eof)))
