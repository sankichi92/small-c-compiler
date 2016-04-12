#lang racket/base
(require rackunit
         "../src/deference-checker.rkt")
(require rackunit/text-ui)
(provide deference-checker-tests)

(define deference-checker-tests
  (test-suite
    "Tests for deference-checker.rkt"

    (check-exn
      exn:fail?
      (lambda ()
        (deference-check-str "void a(){&1;}"))
      "Operator '&' does not have a number as operand")

    (check-exn
      exn:fail?
      (lambda ()
        (deference-check-str "void a(){&a();}"))
      "Operator '&' does not have function as operand")

    (check-not-exn
      (lambda ()
        (deference-check-str "int a[1];void b(int c){int *d;&a[0];&c;&*d;}"))
      "Operator '&' can have an variable/pointer/array as operand")

    (check-exn
      exn:fail?
      (lambda ()
        (deference-check-str "void a(){1 = 1;}"))
      "Operator '=' does not have a number as left operand")

    (check-exn
      exn:fail?
      (lambda ()
        (deference-check-str "int a();void b(){a() = 1;}"))
      "Operator '=' does not have a function as left operand")

    (check-exn
      exn:fail?
      (lambda ()
        (deference-check-str "int a[1];void b(){a = 1;}"))
      "Operator '=' does not have an array as left operand")

    (check-not-exn
      (lambda ()
        (deference-check-str "int a[1];void b(){int c;int *d;a[0]=0;c=1;*d=2;}"))
      "Operator '=' can have a variable as operand")

  ))

(run-tests deference-checker-tests)
