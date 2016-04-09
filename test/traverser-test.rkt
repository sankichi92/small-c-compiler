#lang racket/base
(require rackunit
         parser-tools/lex
         parser-tools/yacc
         "../src/syntax.rkt"
         "../src/traverser.rkt")
(require rackunit/text-ui)
(provide traverser-tests)

(define traverser-tests
  (test-suite
    "Tests for traverser.rkt"

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (var-decl 'a 'int '())))
      (list
        'a)
      "#1")

  ))

(define (decl-proc decl)
  (cond [(var-decl? decl) (var-decl-name decl)]))

(define (stmt-proc stmt) '())

(define (exp-proc exp) '())

(run-tests traverser-tests)
