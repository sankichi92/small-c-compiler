#lang racket/base
(require rackunit
         (prefix-in stx: "../src/syntax.rkt")
         "../src/parser.rkt")
(require rackunit/text-ui)

(define name-resolver-tests
  (test-suite
    "Tests for name-resolver.rkt"
    ))

(run-tests name-resolver-tests)
