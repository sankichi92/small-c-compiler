#lang racket/base
(require rackunit
         "../src/entity.rkt"
         "../src/ir.rkt"
         "../src/code-generator.rkt")
(require rackunit/text-ui)
(provide code-gen-test)

(define code-gen-test
  (test-suite
    "Tests for code-generator.rkt"

    ))

(run-tests code-gen-test)
