#lang racket/base
(require rackunit
         "../src/entity.rkt"
         "../src/ir.rkt"
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide ir-gen-tests)

(define (string->ir str)
  (test-string str #:phase 'ir))

(define ir-gen-tests
  (test-suite
    "Tests for ir-gen.rkt"

    (check-equal?
      (string->ir "int a;")
      (list
        (var-decl (decl 'a 0 'var 'int)))
      "Simple")

    (check-equal?
      (string->ir "void f(){;}")
      (list
        (fun-def
          (decl 'f 0 'fun '(fun void))
          '()
          (cmpd-stmt '() '())))
      "Statement")

    (check-equal?
      (string->ir "int f(int a){return a;}")
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

(run-tests ir-gen-tests)
