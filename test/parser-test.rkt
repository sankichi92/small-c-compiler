#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/syntax.rkt"
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide parser-tests)

(define (parse-string str)
  (test-string str #:phase 'parse))

(define (parse-file fname)
  (test-file fname #:phase 'parse))

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
      "stmt, exp")

    (check-equal?
      (let* ([str "void main(){-1;}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (list
        (aop-exp
          '-
          (lit-exp 0 '())
          (lit-exp 1 (position 14 1 13))
          (position 13 1 12)))
      "Syntax sugar: -1 -> 0-1")

    (check-equal?
      (let* ([str "void main(int a){if(a == 0)a;}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (if-els-stmt
        (list
          (rop-exp
            '==
            (var-exp 'a (position 21 1 20))
            (lit-exp 0 (position 26 1 25))
            (position 23 1 22)))
        (list
          (var-exp 'a (position 28 1 27)))
        '()
        (position 18 1 17))
      "Syntax sugar: if(e)s; -> if(e)s;else;")

    (check-equal?
      (let* ([str "void main(int a){for(a=0;a<10;a=a+1)print(a);}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (cmpd-stmt
        '()
        (list
          (list
            (assign-exp
              (var-exp 'a (position 22 1 21))
              (lit-exp 0 (position 24 1 23))
              (position 23 1 22)))
          (while-stmt
            (list
              (rop-exp
                '<
                (var-exp 'a (position 26 1 25))
                (lit-exp 10 (position 28 1 27))
                (position 27 1 26)))
            (cmpd-stmt
              '()
              (list
                (list
                  (fun-exp
                    'print
                    (list (var-exp 'a (position 43 1 42)))
                    (position 37 1 36)))
                (list
                  (assign-exp
                    (var-exp 'a (position 31 1 30))
                    (aop-exp
                      '+
                      (var-exp 'a (position 33 1 32))
                      (lit-exp 1 (position 35 1 34))
                      (position 34 1 33))
                    (position 32 1 31))))
              '())
            (position 18 1 17)))
        '())
      "Syntax sugar: for(e1;e2;e3)s; -> {e1;while(e2){s;e3;}}")

    (check-equal?
      (let* ([str "void main(int a){for(a=0;a<10;a=a+1){print(a);}}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (cmpd-stmt
        '()
        (list
          (list
            (assign-exp
              (var-exp 'a (position 22 1 21))
              (lit-exp 0 (position 24 1 23))
              (position 23 1 22)))
          (while-stmt
            (list
              (rop-exp
                '<
                (var-exp 'a (position 26 1 25))
                (lit-exp 10 (position 28 1 27))
                (position 27 1 26)))
            (cmpd-stmt
              '()
              (list
                (list
                  (fun-exp
                    'print
                    (list (var-exp 'a (position 44 1 43)))
                    (position 38 1 37)))
                (list
                  (assign-exp
                    (var-exp 'a (position 31 1 30))
                    (aop-exp
                      '+
                      (var-exp 'a (position 33 1 32))
                      (lit-exp 1 (position 35 1 34))
                      (position 34 1 33))
                    (position 32 1 31))))
              (position 37 1 36))
            (position 18 1 17)))
        '())
      "Syntax sugar: for(e1;e2;e3){s}; -> {e1;while(e2){s;e3;}}")

    (check-equal?
      (let* ([str "void main(){int a[2];a[1];}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (list
        (deref-exp
          (list
            (aop-exp
              '+
              (var-exp 'a (position 22 1 21))
              (lit-exp '1 (position 24 1 23))
              '()))
          (position 23 1 22)))
      "Syntax sugar: e1[e2] -> *(e1 + e2)")

    (check-equal?
      (let* ([str "void main(){int *a[2];&a[1];}"]
             [ast (cdr (parse-string str))]
             [cmpd-stmt (fun-def-body (car ast))]
             [stm (car (cmpd-stmt-stmts cmpd-stmt))])
        stm)
      (list
        (list
          (aop-exp
            '+
            (var-exp 'a (position 24 1 23))
            (lit-exp '1 (position 26 1 25))
            '())))
      "Syntax sugar: &e1[e2] -> (e1 + e2)")

    (check-equal?
      (parse-file "pgm/test.sc")
      (list
       (fun-decl 'print 'void '(int) '())
       (fun-def
        'f
        'int
        (list (parm-decl 'x 'int (position 11 1 10)))
        (cmpd-stmt
         '()
         (list
          (while-stmt
           (list
            (rop-exp
             '>
             (var-exp 'x (position 24 2 8))
             (lit-exp 1 (position 28 2 12))
             (position 26 2 10)))
           (cmpd-stmt
            '()
            (list
             (list
              (assign-exp
               (var-exp 'x (position 37 3 4))
               (aop-exp
                '-
                (var-exp 'x (position 41 3 8))
                (lit-exp 2 (position 45 3 12))
                (position 43 3 10))
               (position 39 3 6))))
            (position 31 2 15))
           (position 18 2 2))
          (ret-stmt (list (var-exp 'x (position 61 5 9))) (position 54 5 2)))
         (position 14 1 13))
        (position 1 1 0))
       (fun-def
        'main
        'void
        '()
        (cmpd-stmt
         (list (var-decl 'x 'int (position 87 9 6)))
         (list
          (list
           (fun-exp
            'print
            (list
             (fun-exp 'f (list (lit-exp 9 (position 100 10 10))) (position 98 10 8)))
            (position 92 10 2))))
         (position 79 8 12))
        (position 67 8 0)))
      "test.sc")
  ))

(run-tests parser-tests)
