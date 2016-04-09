#lang racket/base
(require rackunit
         "../src/syntax.rkt"
         "../src/parser.rkt"
         "../src/name-resolver.rkt")
(require rackunit/text-ui)
(provide name-resolver)

(define name-resolver-tests
  (test-suite
    "Tests for name-resolver.rkt"

    (check-equal?
      (name-resolver
        (list
          (var-decl 'a 'int '())))
      (list
        (var-decl (decl 'a 0 'var 'int) 'int '())))

    ))

(run-tests name-resolver-tests)
