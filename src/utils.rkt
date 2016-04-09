#lang racket
(provide (all-defined-out))

(define (err-msg pos msg)
  (format "~a:~a: ~a\n" (position-line pos) (position-col pos) msg))
