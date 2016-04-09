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

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'int
            (list (parm-decl 'b 'int '()))
            (cmpd-stmt
             '()
             (list (ret-stmt (list (var-exp 'b '())) '()))
             '())
            '())))
      (list
        (fun-def
          'a
          'int
          (list 'b)
          (cmpd-stmt
           '()
           (list (ret-stmt (list (var-exp 'b '())) (position 7 7 7)))
           '())
          '()))
      "3")

  ))

(define (decl-proc decl)
  (cond [(var-decl? decl) (var-decl-name decl)]
        [(fun-def? decl) decl]
        [(parm-decl? decl) (parm-decl-name decl)]
        [else decl]))

(define (stmt-proc stmt)
  (cond [(cmpd-stmt? stmt) stmt]
        [(ret-stmt? stmt)
         (ret-stmt (ret-stmt-exp stmt) (position 7 7 7))]
        [else stmt]))

(define (exp-proc exp) exp)

(run-tests traverser-tests)
