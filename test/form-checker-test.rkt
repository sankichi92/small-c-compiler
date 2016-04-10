#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/syntax.rkt"
         "../src/parser.rkt"
         "../src/name-resolver.rkt"
         "../src/form-checker.rkt")
(require rackunit/text-ui)
(provide form-checker-tests)

(define form-checker-tests
  (test-suite
    "Tests for form-checker.rkt"

    (check-exn
      exn:fail?
      (lambda ()
        (form-check-str "void a(){&1;}"))
      "Operator '&' does not have a number as operand")

    (check-exn
      exn:fail?
      (lambda ()
        (form-check-str "void a(){&a();}"))
      "Operator '&' does not have function as operand")

    (check-not-exn
      (lambda ()
        (form-check-str "int a[1];void b(int c){int *d;&a[0];&c;&*d;}"))
      "Operator '&' can have an variable/pointer/array as operand")

  ))

(run-tests form-checker-tests)
