#lang racket
(require parser-tools/lex)
(provide (all-defined-out))

(define (err-msg pos msg)
  (format "~a,~a: ~a" (position-line pos) (position-col pos) msg))

(define (array? type)
  (and (list? type)
       (or (eq? (first type) 'array)
           (array? (second type)))))
