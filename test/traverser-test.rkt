#lang racket/base
(require rackunit
         parser-tools/lex
         parser-tools/yacc
         "../src/syntax.rkt"
         "../src/traverser.rkt")
(require rackunit/text-ui)
(provide traverser-tests)

(define traverser-tests
  (test-suite
    "Tests for traverser.rkt"

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (var-decl 'a 'int '())))
      (list
        'a)
      "1")

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'void
            (list (parm-decl 'b 'int '()))
            (cmpd-stmt '() '(()) '())
            '())))
      (list
        (fun-def
          'a
          'void
          (list 'b)
          (cmpd-stmt '() '(()) '())
          '()))
      "2")

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'int
            (list (parm-decl 'b 'int '()))
            (cmpd-stmt
             '()
             (list (ret-stmt (list (var-exp 'b '())) '()))
             '())
            '())))
      (list
        (fun-def
          'a
          'int
          (list 'b)
          (cmpd-stmt
           '()
           (list (ret-stmt (list (var-exp 'b '())) (position 7 7 7)))
           '())
          '()))
      "3")

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'void
            '()
            (cmpd-stmt '() (list (list (lit-exp 0 '()))) '())
            '())))
      (list
        (fun-def
          'a
          'void
          '()
          (cmpd-stmt '() (list (list (lit-exp 1 '())))  '())
          '()))
      "4")

    (check-equal?
      (traverser
        decl-proc
        stmt-proc
        exp-proc
        (list
          (fun-def
            'a
            'void
            '()
            (cmpd-stmt
              (list (var-decl 'b 'int (position 14 1 13)))
              (list
                (list
                  (assign-exp
                    (var-exp 'b (position 16 1 15))
                    (lit-exp 0 (position 18 1 17))
                    (position 17 1 16))))
              (position 9 1 8))
            (position 1 1 0))))
      (list
        (fun-def
          'a
          'void
          '()
          (cmpd-stmt
            (list 'b)
            (list
              (list
                (assign-exp
                  (var-exp 'b (position 16 1 15))
                  (lit-exp 1 (position 18 1 17))
                  (position 17 1 16))))
            (position 9 1 8))
          (position 1 1 0)))
      "5")

  ))

(define (decl-proc decl)
  (cond [(var-decl? decl) (var-decl-name decl)]
        [(parm-decl? decl) (parm-decl-name decl)]
        [else decl]))

(define (stmt-proc stmt)
  (cond [(ret-stmt? stmt)
         (ret-stmt (ret-stmt-exp stmt) (position 7 7 7))]
        [else stmt]))

(define (exp-proc exp)
  (cond [(lit-exp? exp)
         (lit-exp
           (+ (lit-exp-val exp) 1)
           (lit-exp-pos exp))]
        [else exp]))

(run-tests traverser-tests)
