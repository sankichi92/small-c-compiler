#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/entity.rkt"
         "../src/ir.rkt"
         "../src/ir-generator.rkt")
(require rackunit/text-ui)
(provide ir-generator-tests)

(define ir-generator-tests
  (test-suite
    "Tests for ir-generator.rkt"

    (check-equal?
      (string->ir "int a;")
      (list
        (fun-def (decl 'print 0 'proto '(fun void int)) '() '())
        (var-decl (decl 'a 0 'var 'int)))
      "Simple")

    (check-equal?
      (cdr (string->ir "void f(){;}"))
      (list
        (fun-def
          (decl 'f 0 'fun '(fun void))
          '()
          (cmpd-stmt '() '())))
      "Statement")

    (check-equal?
      (cdr (string->ir "int f(int a){return a;}"))
      (list
        (fun-def
          (decl 'f 0 'fun '(fun int int))
          (list (var-decl (decl 'a 1 'parm 'int)))
          (cmpd-stmt
           (list (var-decl (decl '_x0 '() 'temp 'temp)))
           (list
            (assign-stmt (decl '_x0 '() 'temp 'temp) (var-exp (decl 'a 1 'parm 'int)))
            (ret-stmt (decl '_x0 '() 'temp 'temp))))))
      "Statement")

    (check-equal?
      (cdr (string->ir "int f(int a){if(a>0)a;else 0;}"))
      (list
        (fun-def
          (decl 'f 0 'fun '(fun int int))
          (list (var-decl (decl 'a 1 'parm 'int)))
          (cmpd-stmt
           (list (var-decl (decl '_x0 '() 'temp 'temp)))
           (list
            (assign-stmt (decl '_x0 '() 'temp 'temp) (var-exp (decl 'a 1 'parm 'int)))
            (ret-stmt (decl '_x0 '() 'temp 'temp))))))
      "Statement")

    ))

(run-tests ir-generator-tests)
