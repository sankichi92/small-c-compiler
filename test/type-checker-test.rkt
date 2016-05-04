#lang racket/base
(require rackunit
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide type-checker-tests)

(define (well-typed? sym)
  (eq? sym 'well-typed))

(define (check-string str)
  (test-string str #:phase 'ty-check))

(define (check-file fname)
  (test-file fname #:phase 'ty-check))

(define type-checker-tests
  (test-suite
    "Tests for type-checker.rkt"

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void a;"))
      "variable has incomplete type 'void'")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void a[0];"))
      "array has incomplete element type 'void'")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void *p;"))
      "pointer has incomplete type 'void'")

    (check-pred
      well-typed?
      (car (check-string "int a;"))
      "'int a;' is well-typed")

    (check-pred
      well-typed?
      (car (check-string "int main(){;}"))
      "well-typed")

    (check-pred
      well-typed?
      (car (check-string "void f(){0,1;}"))
      "well-typed")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "int main(){int a;return &a;}"))
      "incompatible returning 'int*' from a function with result type 'int'")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void *f(){}"))
      "pointer has incomplete type 'void'")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void f(){int *a;a=0;}"))
      "incompatible assigning to 'int*' from 'int'")

    (check-pred
      well-typed?
      (car (check-string "int a[1];int g(int a){a=0;return a==1;}int *f(){int b;a[0]=1;b=a[0]+1;return &b;}"))
      "well-typed")

    (check-pred
      well-typed?
      (car (check-file "pgm/test.sc"))
      "test.sc")

    (check-pred
      well-typed?
      (car (check-file "pgm/sort.sc"))
      "sort.sc")

  ))

(run-tests type-checker-tests)
