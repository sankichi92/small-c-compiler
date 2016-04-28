#lang racket/base
(require rackunit
         "../src/entity.rkt"
         "../src/ir.rkt"
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide addr-assigner-tests)

(define (assign-string str)
  (test-string str #:phase 'addr))

(define addr-assigner-tests
  (test-suite
    "Tests for addr-assigner.rkt"

    (check-equal?
      (cdr (assign-string "void f(int a,int b,int c,int *d,int *e,int *f){int x,y[4];;}"))
      (list
       (fun-def
        (decl
         'f
         0
         'fun
         '(fun void int int int (pointer int) (pointer int) (pointer int)))
        (list
         (var-decl (decl 'a 1 'parm 'int))
         (var-decl (decl 'b 1 'parm 'int))
         (var-decl (decl 'c 1 'parm 'int))
         (var-decl (decl 'd 1 'parm '(pointer int)))
         (var-decl (let ([obj (decl 'e 1 'parm '(pointer int))])
                     (set-decl-offset! obj 4)
                     obj))
         (var-decl (let ([obj (decl 'f 1 'parm '(pointer int))])
                     (set-decl-offset! obj 8)
                     obj)))
        (cmpd-stmt
         (list
          (var-decl (let ([obj (decl 'x 2 'var 'int)])
                      (set-decl-offset! obj 0)
                      obj))
          (var-decl (let ([obj (decl 'y 2 'var '(array int 4))])
                      (set-decl-offset! obj -16)
                      obj)))
         '())))
      "Simple")

    ))

(run-tests addr-assigner-tests)
