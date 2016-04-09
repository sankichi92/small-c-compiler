#lang racket/base
(require rackunit
         parser-tools/lex
         parser-tools/yacc
         "../src/syntax.rkt"
         "../src/parser.rkt")
(require rackunit/text-ui)
(provide parser-tests)

(define parser-tests
  (test-suite
    "Tests for parser.rkt"

    (check-equal?
      (parse-string "int a;")
      (list
        (fun-decl 'print 'void '(int) '())
        (var-decl 'a 'int (position 5 1 4)))
      "var-decl")

    (check-equal?
      (cdr (parse-string "int a();"))
      (list
        (fun-decl 'a 'int '() (position 1 1 0)))
      "fun-decl")

    (check-equal?
      (cdr (parse-string "void a(){;}"))
      (list
        (fun-def
          'a
          'void
          '()
          (cmpd-stmt '() '(()) (position 9 1 8))
          (position 1 1 0)))
      "fun-def, cmpd-stmt")

    (check-equal?
      (cdr (parse-string "int a(int b){return b;}"))
      (list
        (fun-def
          'a
          'int
          (list (parm-decl 'b 'int (position 11 1 10)))
          (cmpd-stmt
            '()
            (list
              (ret-stmt
                (list
                  (var-exp 'b (position 21 1 20)))
                (position 14 1 13)))
            (position 13 1 12))
          (position 1 1 0)))
      "fun-def, cmpd-stmt")
  ))

(run-tests parser-tests)
