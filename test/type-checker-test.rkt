#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/syntax.rkt"
         "../src/entity.rkt"
         "../src/type-checker.rkt")
(require rackunit/text-ui)
(provide type-checker-tests)

(define type-checker-tests
  (test-suite
    "Tests for type-checker.rkt"

    (check-pred
      well-typed?
      (type-check
        (list 'well-typed 'well-typed))
      "program")

  ))

(run-tests type-checker-tests)
