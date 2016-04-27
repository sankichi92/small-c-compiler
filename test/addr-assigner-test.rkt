#lang racket/base
(require rackunit
         "../src/entity.rkt"
         "../src/ir.rkt"
         "../src/addr-assigner.rkt")
(require rackunit/text-ui)
(provide ir-generator-tests)

(define addr-assigner-tests
  (test-suite
    "Tests for addr-assigner.rkt"

    ))

(run-tests addr-assigner-tests)
