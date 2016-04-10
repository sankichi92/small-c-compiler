#lang racket/base
(require rackunit
         parser-tools/lex
         "../src/syntax.rkt"
         "../src/name-resolver.rkt")
(require rackunit/text-ui)
(provide name-resolver-tests)

(define name-resolver-tests
  (test-suite
    "Tests for name-resolver.rkt"

    (check-equal?
      (name-resolve
        (list
          (var-decl 'a 'int '())))
      (list
        (var-decl (decl 'a 0 'var 'int) 'int '()))
      "var-decl-name -> obj")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a();int a;"))
      "void a();int a; -> redifinition error")

    (check-not-exn
      (lambda ()
        (name-resolve-str "void a(){int a;}"))
      "no error")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a(int a, int a){;}"))
      "void a(int a, int a){;} -> parm redifinition error")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a(); void a(int b){;}"))
      "redifinition error")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a(){;} void a(){;}"))
      "redifinition error")

    (check-not-exn
      (lambda ()
        (name-resolve-str "void a(); void a(); void a(){;}"))
      "no error")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a(){a;}"))
      "reference error")

    (check-exn
      exn:fail?
      (lambda ()
        (name-resolve-str "void a(int a){a(a);}"))
      "reference rror")

    (check-equal?
      (cdr (name-resolve-str "int a;void f(int a){a;}void g(){a;}"))
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
      (cdr (name-resolve-str "void f(){int a;{int a;a;}a;}"))
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
    ))

(run-tests name-resolver-tests)
