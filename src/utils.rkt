#lang racket
(require parser-tools/lex)
(provide (all-defined-out))

(define (err-msg pos msg)
  (format "~a,~a: ~a" (position-line pos) (position-col pos) msg))
