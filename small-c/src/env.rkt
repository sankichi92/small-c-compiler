#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in parse: "parser.rkt"))
(provide (all-defined-out))

(struct decl (name lev kind type) #:transparent)

(define (name-resolver ast)
  (define initial-env (lambda (x) #f))
  (define (register env decl)
    (lambda (name)
      (if (eq? name (decl-name decl))
          decl
          (env name))))
  (define (resolve-list env lev list resolver)
    (if (null? list)
        (cons '() env)
        (let* ([item (car list)]
               [ret (resolver env lev item)]
               [new-item (car ret)]
               [new-env (cdr ret)]
               [rest-ret (resolve-list new-env lev (cdr list) resolver)]
               [last-env (cdr rest-ret)])
          (cons (cons new-item (car rest-ret)) last-env))))
  (define (resolve-decl env lev decl)
    (cond [(stx:var-decls? decl)
           (let ([ty (stx:var-decls-ty decl)]
                 [var-list (stx:var-decls-decls decl)])
             (resolve-var-decl-list env lev var-list ty))]
          [(stx:fun-decl? decl)
           (let* ([name (stx:fun-decl-name decl)]
                  [ret-ty (stx:fun-decl-ret-ty decl)]
                  [parm-tys (stx:fun-decl-parm-tys decl)]
                  [pos (stx:fun-decl-pos decl)]
                  [ret (env name)])
             (if ret
                 (let ([kind (decl-kind ret)]
                       [type (decl-type ret)])
                   (cond [(and (eq? kind 'var))
                          (redef-err pos name)]
                         [(and
                           (or (eq? kind 'proto) (eq? kind 'fun))
                           (not (equal? parm-tys (cddr type))))
                          (redef-err pos name)]
                         [(or (eq? kind 'proto) (eq? kind 'fun))
                          (cons '() env)]
                         [else
                          (register-proto env name ret-ty parm-tys pos)]))
                 (register-proto env name ret-ty parm-tys pos)))]
          [(stx:fun-def? decl)
           (let* ([name (stx:fun-def-name decl)]
                  [ret-ty (stx:fun-def-ret-ty decl)]
                  [parms (stx:fun-def-parms decl)]
                  [body (stx:fun-def-body decl)]
                  [pos (stx:fun-def-pos decl)]
                  [parms-lev (+ lev 1)]
                  [ret1 (resolve-list env parms-lev parms resolve-parm-decl)]
                  [new-parms (car ret1)]
                  [parms-env (cdr ret1)]
                  [ret2 (resolve-cmpd-stmt parms-env parms-lev body)]
                  [new-body (car ret2)]
                  [ret (parms-env name)])
             (if ret
                 (let ([kind (decl-kind ret)]
                       [type (decl-type ret)])
                   (cond [(or (eq? kind 'var) (eq? kind 'fun))
                          (redef-err pos name)]
                         [(and
                           (eq? kind 'proto)
                           (not (equal? (map stx:parm-decl-ty parms) (cddr type))))
                          (redef-err pos name)]
                         [(eq? kind 'fun)
                          (cons '() parms-env)]
                         [else
                          (register-fun env name ret-ty new-parms new-body pos)]))
                 (register-fun env name ret-ty new-parms new-body pos)))]))
  (define (resolve-var-decl-list env lev decl-list ty)
    (if (null? decl-list)
        (cons '() env)
        (let* ([var (car decl-list)]
               [ret (resolve-var-decl env lev var ty)]
               [new-var (car ret)]
               [new-env (cdr ret)]
               [rest-ret (resolve-var-decl-list new-env lev (cdr decl-list) ty)]
               [last-env (cdr rest-ret)])
          (cons (cons new-var (car rest-ret)) last-env))))
  (define (resolve-var-decl env lev var ty1)
    (let* ([name (stx:var-decl-name var)]
           [ty2 (stx:var-decl-ty var)]
           [ty (format-type ty1 ty2)]
           [pos (stx:var-decl-pos var)]
           [ret (env name)])
      (if ret
          (let ([ret-kind (decl-kind ret)]
                [ret-lev (decl-lev ret)])
            (cond [(or (eq? ret-kind 'fun) (eq? ret-kind 'proto))
                   (redef-err pos name)]
                  [(and (eq? ret-kind 'var) (= ret-lev lev))
                   (redef-err pos name)]
                  [else
                   (begin
                     (cond [(eq? ret-kind 'parm)
                            (redef-warn pos name)])
                     (register-var env lev name ty pos))]))
          (register-var env lev name ty pos))))
  (define (resolve-parm-decl env lev parm)
    (let* ([name (stx:parm-decl-name parm)]
           [ty (stx:parm-decl-ty parm)]
           [pos (stx:parm-decl-pos parm)]
           [ret (env name)])
      (if ret
          (let ([kind (decl-kind ret)])
            (if (eq? kind 'parm)
                (redef-err pos name)
                (register-parm env name ty pos)))
          (register-parm env name ty pos))))
  (define (resolve-cmpd-stmt env prev-lev cmpd-stmt)
    (let* ([lev (+ prev-lev 1)]
           [decl-list (stx:cmpd-stmt-decls cmpd-stmt)]
           [stmt-list (stx:cmpd-stmt-stmts cmpd-stmt)]
           [pos (stx:cmpd-stmt-pos cmpd-stmt)]
           [ret1 (resolve-list env lev decl-list resolve-in-decl)]
           [new-decl-list (car ret1)]
           [new-env (cdr ret1)]
           ;[ret2 (resolve-list env lev stmt-list resolve-stmt)]
           ;[new-stmt-list (car ret1)]
           )
      (cons (stx:cmpd-stmt new-decl-list stmt-list pos) new-env)))
  (define (resolve-in-decl env lev decl)
    (let ([ty (stx:var-decls-ty decl)]
          [var-list (stx:var-decls-decls decl)])
      (resolve-var-decl-list env lev var-list ty)))
  (define (register-var env lev name ty pos)
    (let* ([decl (decl name lev 'var ty)]
           [new-env (register env decl)])
      (cons (stx:var-decl decl ty pos) new-env)))
  (define (register-proto env name ret-ty parm-tys pos)
    (let* ([decl (decl name 0 'proto (list* 'fun ret-ty parm-tys))]
           [new-env (register env decl)])
      (cons (stx:fun-decl decl ret-ty parm-tys pos) new-env)))
  (define (register-fun env name ret-ty parms body pos)
    (let* ([parm-tys (map stx:parm-decl-ty parms)]
           [type (list* 'fun ret-ty parm-tys)]
           [decl (decl name 0 'fun type)]
           [new-env (register env decl)])
      (cons (stx:fun-def decl ret-ty parms body pos) new-env)))
  (define (register-parm env name ty pos)
    (let* ([decl (decl name 1 'parm ty)]
           [new-env (register env decl)])
      (cons (stx:parm-decl decl ty pos) new-env)))
  (define (format-type ty1 ty2)
    (cond [(null? ty2)
           ty1]
          [(eq? 'array (car ty2))
           (list 'array ty1 (cdr ty2))]
          [(eq? 'pointer (car ty2))
           (list 'pointer (format-type ty1 (cdr ty2)))]))
  (define (redef-warn pos name)
    (eprintf (err-msg pos (format "name resolve warning: overwriting of parm ~a" name))))
  (define (redef-err pos name)
    (error (err-msg pos (format "name resolve error: redifinition of ~a" name))))
  (car (resolve-list initial-env 0 ast resolve-decl)))

(define (err-msg pos msg)
  (format "~a:~a: ~a\n" (position-line pos) (position-col pos) msg))
