#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/syntax.rkt"
         "../src/entity.rkt"
         "../src/compiler.rkt")
(require rackunit/text-ui)
(provide name-resolver-tests)

(define (resolve-string str)
  (test-string str #:phase 'resolve))

(define (resolve-file fname)
  (test-file fname #:phase 'resolve))

(define name-resolver-tests
  (test-suite
    "Tests for name-resolver.rkt"

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a();int a;"))
      "void a();int a; -> redifinition error")

    (check-not-exn
      (lambda ()
        (resolve-string "void a(){int a;}"))
      "no error")

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a(int a, int a){;}"))
      "void a(int a, int a){;} -> parm redifinition error")

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a(); void a(int b){;}"))
      "redifinition error")

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a(){;} void a(){;}"))
      "redifinition error")

    (check-not-exn
      (lambda ()
        (resolve-string "void a(); void a(); void a(){;}"))
      "no error")

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a(){a;}"))
      "reference error")

    (check-exn
      exn:fail?
      (lambda ()
        (resolve-string "void a(int a){a(a);}"))
      "reference rror")

    (check-equal?
      (cdr (resolve-string "int a;void f(int a){a;}void g(){a;}"))
      (list
       (var-decl (decl 'a 0 'var 'int) 'int (position 5 1 4))
       (fun-def
        (decl 'f 0 'fun '(fun void int))
        'void
        (list (parm-decl (decl 'a 1 'parm 'int) 'int (position 18 1 17)))
        (cmpd-stmt
         '()
         (list (list (var-exp (decl 'a 1 'parm 'int) (position 21 1 20))))
         (position 20 1 19))
        (position 7 1 6))
       (fun-def
        (decl 'g 0 'fun '(fun void))
        'void
        '()
        (cmpd-stmt
         '()
         (list (list (var-exp (decl 'a 0 'var 'int) (position 33 1 32))))
         (position 32 1 31))
        (position 24 1 23)))
      "parm scope")

    (check-equal?
      (cdr (resolve-string "void f(){int a;{int a;a;}a;}"))
      (list
       (fun-def
        (decl 'f 0 'fun '(fun void))
        'void
        '()
        (cmpd-stmt
         (list (var-decl (decl 'a 2 'var 'int) 'int (position 14 1 13)))
         (list
          (cmpd-stmt
           (list (var-decl (decl 'a 3 'var 'int) 'int (position 21 1 20)))
           (list (list (var-exp (decl 'a 3 'var 'int) (position 23 1 22))))
           (position 16 1 15))
          (list (var-exp (decl 'a 2 'var 'int) (position 26 1 25))))
         (position 9 1 8))
        (position 1 1 0)))
      "smpd-stmt scope")

    (check-equal?
      (resolve-file "pgm/test.sc")
      (list
       (fun-decl (decl 'print 0 'proto '(fun void int)) 'void '(int) '())
       (fun-def
        (decl 'f 0 'fun '(fun int int))
        'int
        (list (parm-decl (decl 'x 1 'parm 'int) 'int (position 11 1 10)))
        (cmpd-stmt
         '()
         (list
          (while-stmt
           (list
            (rop-exp
             '>
             (var-exp (decl 'x 1 'parm 'int) (position 24 2 8))
             (lit-exp 1 (position 28 2 12))
             (position 26 2 10)))
           (cmpd-stmt
            '()
            (list
             (list
              (assign-exp
               (var-exp (decl 'x 1 'parm 'int) (position 37 3 4))
               (aop-exp
                '-
                (var-exp (decl 'x 1 'parm 'int) (position 41 3 8))
                (lit-exp 2 (position 45 3 12))
                (position 43 3 10))
               (position 39 3 6))))
            (position 31 2 15))
           (position 18 2 2))
          (ret-stmt
           (list (var-exp (decl 'x 1 'parm 'int) (position 61 5 9)))
           (position 54 5 2)))
         (position 14 1 13))
        (position 1 1 0))
       (fun-def
        (decl 'main 0 'fun '(fun void))
        'void
        '()
        (cmpd-stmt
         (list (var-decl (decl 'x 2 'var 'int) 'int (position 87 9 6)))
         (list
          (list
           (fun-exp
            (decl 'print 0 'proto '(fun void int))
            (list
             (fun-exp
              (decl 'f 0 'fun '(fun int int))
              (list (lit-exp 9 (position 100 10 10)))
              (position 98 10 8)))
            (position 92 10 2))))
         (position 79 8 12))
        (position 67 8 0)))
      "test.sc")

    ))

(run-tests name-resolver-tests)
