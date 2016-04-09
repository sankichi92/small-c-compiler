#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt"))
(provide (all-defined-out))

(define (traverser decl-proc stmt-proc exp-proc ast)
  (define (traverse-decl decl)
    (cond [(stx:var-decl? decl) decl]))
  (map decl-proc (map traverse-decl ast)))
