#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-defined-out))

(define-empty-tokens tokens-without-value
  (+ * EOF))

(define-tokens tokens-with-value
  (NUM))

(define-lex-abbrevs
  (digit   (char-range "0" "9")))

(define calc-lexer
  (lexer-src-pos
   ((:+ digit) (token-NUM (string->number lexeme)))
   ("+"        (token-+))
   ("*"        (token-*))
   (whitespace (return-without-pos (calc-lexer input-port)))
   ((eof)      (token-EOF))))
