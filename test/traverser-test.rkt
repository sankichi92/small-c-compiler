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
      "1")

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'void
            (list (parm-decl 'b 'int '()))
            (cmpd-stmt '() '(()) '())
            '())))
      (list
        (fun-def
          'a
          'void
          (list 'b)
          (cmpd-stmt '() '(()) '())
          '()))
      "2")

  ))

(define (decl-proc decl)
  (cond [(var-decl? decl) (var-decl-name decl)]
        [(fun-def? decl) decl]
        [(parm-decl? decl) (parm-decl-name decl)]))

(define (stmt-proc stmt) stmt)

(define (exp-proc exp) exp)

(run-tests traverser-tests)
