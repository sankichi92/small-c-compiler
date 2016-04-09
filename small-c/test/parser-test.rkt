#lang racket/base
(require rackunit
         (prefix-in stx: "../src/syntax.rkt")
         "../src/parser.rkt")
(require rackunit/text-ui)

(define parser-tests
  (test-suite
    "Tests for parser.rkt"
    ))

(run-tests parser-tests)
