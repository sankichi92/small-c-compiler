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

    ;(check-pred
    ;  well-typed?
    ;  (type-check
    ;    (list 'well-typed 'well-typed))
    ;  "program")

    (check-exn
      exn:fail?
      (lambda ()
        (type-check-str "void a;"))
      "variable has incomplete type 'void'")

    (check-exn
      exn:fail?
      (lambda ()
        (type-check-str "void a[0];"))
      "array has incomplete element type 'void'")

    (check-exn
      exn:fail?
      (lambda ()
        (type-check-str "void *p;"))
      "pointer has incomplete type 'void'")

    (check-pred
      well-typed?
      (type-check-str "int a;")
      "'int a;' is well-typed")

    (check-pred
      well-typed?
      (type-check-str "int main(){;}")
      "well-typed")

    (check-pred
      well-typed?
      (type-check-str "void f(){0,1;}")
      "well-typed")

    (check-exn
      exn:fail?
      ;(lambda ()
        (type-check-str "int main(){int a;return &a;}");)
      "")

    (check-exn
      exn:fail?
      (lambda ()
        (type-check-str "void *f(){}"))
      "")

  ))

(run-tests type-checker-tests)
