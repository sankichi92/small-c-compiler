#lang racket/base
(require rackunit
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide deference-checker-tests)

(define (check-string str)
  (test-string str #:phase 'def-check))

(define (check-file fname)
  (test-file fname #:phase 'def-check))

(define deference-checker-tests
  (test-suite
    "Tests for deference-checker.rkt"

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void a(){&1;}"))
      "Operator '&' does not have a number as operand")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void a(){&a();}"))
      "Operator '&' does not have function as operand")

    (check-not-exn
      (lambda ()
        (check-string "int a[1];void b(int c){int *d;&a[0];&c;&*d;}"))
      "Operator '&' can have an variable/pointer/array as operand")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "void a(){1 = 1;}"))
      "Operator '=' does not have a number as left operand")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "int a();void b(){a() = 1;}"))
      "Operator '=' does not have a function as left operand")

    (check-exn
      exn:fail?
      (lambda ()
        (check-string "int a[1];void b(){a = 1;}"))
      "Operator '=' does not have an array as left operand")

    (check-not-exn
      (lambda ()
        (check-string "int a[1];void b(){int c;int *d;a[0]=0;c=1;*d=2;}"))
      "Operator '=' can have a variable as operand")

    (check-not-exn
      (lambda ()
        (check-file "program/test.sc"))
      "test.sc")

    (check-not-exn
      (lambda ()
        (check-file "program/sort.sc"))
      "sort.sc")

  ))

(run-tests deference-checker-tests)
