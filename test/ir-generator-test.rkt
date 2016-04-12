#lang racket/base
(require rackunit
         "ir.rkt"
         "../src/ir-generator.rkt")
(require rackunit/text-ui)
(provide ir-generator-tests)

(define ir-generator-tests
  (test-suite
    "Tests for ir-generator.rkt"

    ))

(run-tests ir-generator-tests)
