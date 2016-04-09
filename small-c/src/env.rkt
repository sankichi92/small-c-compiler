#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt"))
(provide (all-defined-out))

(struct decl (name lev kind type) #:transparent)

(define (name-resolver ast)
  (define initial-env (lambda (x) #f))
  (define (register env decl)
    (lambda (name)
      (if (eq? name (decl-name decl))
          decl
          (env name))))
  (define (resolve-decl-list env lev decl-list resolver)
    (if (null? decl-list)
        (cons '() env)
        (let* ([item (car decl-list)]
               [ret (resolver env lev item)]
               [new-item (car ret)]
               [new-env (cdr ret)]
               [rest-ret (resolve-decl-list new-env lev (cdr decl-list) resolver)]
               [last-env (cdr rest-ret)])
          (cons (cons new-item (car rest-ret)) last-env))))
  (define (resolve-decl env lev decl)
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
                    (if (or
                          (eq? kind 'var)
                          (and
                            (or (eq? kind 'proto) (eq? kind 'fun))
                            (not (equal? parm-tys (cddr type)))))
                        (redef-err pos name)
                        (register-proto env name ret-ty parm-tys pos)))
                  (register-proto env name ret-ty parm-tys pos)))]
          [(stx:fun-def? decl)
           (let* ([name (stx:fun-def-name decl)]
                  [ret-ty (stx:fun-def-ret-ty decl)]
                  [parms (stx:fun-def-parms decl)]
                  [body (stx:fun-def-body decl)]
                  [pos (stx:fun-def-pos decl)]
                  [parms-lev (+ lev 1)]
                  [ret1 (resolve-decl-list env parms-lev parms resolve-parm-decl)]
                  [new-parms (car ret1)]
                  [parms-env (cdr ret1)]
                  [new-body (resolve-cmpd-stmt parms-env parms-lev body)]
                  [ret2 (env name)])
              (if ret2
                  (let ([kind (decl-kind ret2)]
                        [type (decl-type ret2)])
                    (if (or
                          (eq? kind 'var)
                          (eq? kind 'fun)
                          (and
                            (eq? kind 'proto)
                            (not (equal? (map stx:parm-decl-ty parms) (cddr type)))))
                        (redef-err pos name)
                        (register-fun env name ret-ty new-parms new-body pos)))
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
            (if (or
                  (eq? ret-kind 'fun)
                  (eq? ret-kind 'proto)
                  (and (eq? ret-kind 'var) (= ret-lev lev)))
                (redef-err pos name)
                (begin
                  (cond [(eq? ret-kind 'parm)
                         (redef-warn pos name)])
                  (register-var env lev name ty pos))))
          (register-var env lev name ty pos))))
  (define (resolve-stmt-list env lev stmt-list)
  (map (lambda (stmt)
         (resolve-stmt env lev stmt))
       stmt-list))
  (define (resolve-stmt env lev stmt)
    (cond [(null? stmt) '()]
          [(list? stmt) (resolve-exp-list env lev stmt)]
          [(stx:if-els-stmt? stmt)
           (let* ([test (stx:if-els-stmt-test stmt)]
                  [tbody (stx:if-els-stmt-tbody stmt)]
                  [ebody (stx:if-els-stmt-ebody stmt)]
                  [pos (stx:if-els-stmt-pos stmt)]
                  [new-test (resolve-exp-list env lev test)]
                  [new-tbody (resolve-stmt env lev tbody)]
                  [new-ebody (resolve-stmt env lev ebody)])
              (stx:if-els-stmt new-test new-tbody new-ebody pos))]
          [(stx:while-stmt? stmt)
           (let* ([test (stx:while-stmt-test stmt)]
                  [body (stx:while-stmt-body stmt)]
                  [pos (stx:while-stmt-pos stmt)]
                  [new-test (resolve-exp-list env lev test)]
                  [new-body (resolve-stmt env lev body)])
              (stx:while-stmt new-test new-body pos))]
          [(stx:ret-stmt? stmt)
           (let* ([exp (stx:ret-stmt-exp stmt)]
                  [pos (stx:ret-stmt-pos stmt)]
                  [new-exp (resolve-exp-list env lev exp)])
              (stx:ret-stmt new-exp pos))]
          [(stx:cmpd-stmt? stmt) (resolve-cmpd-stmt env lev stmt)]))
  (define (resolve-cmpd-stmt env prev-lev cmpd-stmt)
    (define (resolve-in-decl env lev decl)
      (let ([ty (stx:var-decls-ty decl)]
            [var-list (stx:var-decls-decls decl)])
        (resolve-var-decl-list env lev var-list ty)))
    (let* ([lev (+ prev-lev 1)]
           [decl-list (stx:cmpd-stmt-decls cmpd-stmt)]
           [stmt-list (stx:cmpd-stmt-stmts cmpd-stmt)]
           [pos (stx:cmpd-stmt-pos cmpd-stmt)]
           [ret1 (resolve-decl-list env lev decl-list resolve-in-decl)]
           [new-decl-list (car ret1)]
           [new-env (cdr ret1)]
           [new-stmt-list (resolve-stmt-list env lev stmt-list)])
      (cons (stx:cmpd-stmt new-decl-list new-stmt-list pos) new-env)))
  (define (resolve-exp-list env lev exp-list)
    (map (lambda (exp)
           (resolve-exp env lev exp))
         exp-list))
  (define (resolve-exp env lev exp)
    (cond [(stx:assign-exp? exp)
           (let ([left (resolve-exp env lev (stx:assign-exp-left exp))]
                 [right (resolve-exp env lev (stx:assign-exp-right exp))]
                 [pos (stx:assign-exp-pos exp)])
              (stx:assign-exp left right pos))]
          [(stx:lop-exp? exp)
           (let ([op (stx:lop-exp-op exp)]
                 [left (resolve-exp env lev (stx:lop-exp-left exp))]
                 [right (resolve-exp env lev (stx:lop-exp-right exp))]
                 [pos (stx:lop-exp-pos exp)])
              (stx:lop-exp op left right pos))]
          [(stx:rop-exp? exp)
           (let ([op (stx:rop-exp-op exp)]
                 [left (resolve-exp env lev (stx:rop-exp-left exp))]
                 [right (resolve-exp env lev (stx:rop-exp-right exp))]
                 [pos (stx:rop-exp-pos exp)])
              (stx:rop-exp op left right pos))]
          [(stx:aop-exp? exp)
           (let ([op (stx:aop-exp-op exp)]
                 [left (resolve-exp env lev (stx:aop-exp-left exp))]
                 [right (resolve-exp env lev (stx:aop-exp-right exp))]
                 [pos (stx:aop-exp-pos exp)])
              (stx:aop-exp op left right pos))]
          [(stx:addr-exp? exp)
           (let ([var (resolve-exp env lev (stx:addr-exp-var exp))]
                 [pos (stx:addr-exp-pos exp)])
              (stx:addr-exp var pos))]
          [(stx:deref-exp? exp)
           (let ([arg (resolve-exp env lev (stx:deref-exp-arg exp))]
                 [pos (stx:deref-exp-pos exp)])
              (stx:deref-exp arg pos))]
          [(stx:fun-exp? exp)
           (let* ([name (stx:fun-exp-name exp)]
                  [args (resolve-exp-list env lev (stx:fun-exp-args exp))]
                  [pos (stx:fun-exp-pos exp)]
                  [decl (env name)])
              (if decl
                  (stx:fun-exp decl args pos)
                  (unknown-err pos name)))]
          [(stx:var-exp? exp)
           (let* ([name (stx:var-exp-tgt exp)]
                  [pos (stx:var-exp-pos exp)]
                  [decl (env name)])
              (if decl
                  (stx:var-exp decl pos)
                  (unknown-err pos name)))]
          [(stx:lit-exp? exp)]))
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
  (define (unknown-err pos name)
    (error (err-msg pos (format "name resolve error: unknown identifier ~a" name))))
  (car (resolve-decl-list initial-env 0 ast resolve-decl)))

(define (err-msg pos msg)
  (format "~a:~a: ~a\n" (position-line pos) (position-col pos) msg))
