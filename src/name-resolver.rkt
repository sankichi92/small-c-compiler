#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         "utils.rkt"
         "parser.rkt")
(provide (all-defined-out))

(struct decl (name lev kind type) #:transparent)

(define (name-resolve ast)
  (define initial-env (lambda (x) #f))
  (define (register env decl)
    (lambda (name)
      (if (eq? name (decl-name decl))
          decl
          (env name))))
  (define (resolve-decl-list env lev decl-list)
    (if (null? decl-list)
        (cons '() env)
        (let* ([decl (car decl-list)]
               [ret (resolve-decl env lev decl)]
               [new-decl (car ret)]
               [new-env (cdr ret)]
               [rest-ret (resolve-decl-list new-env lev (cdr decl-list))]
               [last-env (cdr rest-ret)])
          (cons (cons new-decl (car rest-ret)) last-env))))
  (define (resolve-decl env lev decl)
    (match decl
      [(stx:var-decl name ty pos)
       (let ([ret (env name)])
         (if ret
             (let ([kind (decl-kind ret)]
                   [ret-lev (decl-lev ret)])
               (if (or
                     (eq? kind 'fun)
                     (eq? kind 'proto)
                     (and (eq? kind 'var) (= ret-lev lev)))
                   (redef-err pos name)
                   (begin
                     (cond [(eq? kind 'parm)
                            (redef-warn pos name)])
                     (register-var env lev name ty pos))))
             (register-var env lev name ty pos)))]
      [(stx:fun-decl name ret-ty parm-tys pos)
       (let ([ret (env name)])
         (if ret
             (let ([kind (decl-kind ret)]
                   [type (decl-type ret)])
               (if (or
                     (eq? kind 'var)
                     (and
                       (or (eq? kind 'proto) (eq? kind 'fun))
                       (not (equal? (list* 'fun ret-ty parm-tys) type))))
                   (redef-err pos name)
                   (register-proto env name ret-ty parm-tys pos)))
             (register-proto env name ret-ty parm-tys pos)))]
      [(stx:fun-def name ret-ty parms body pos)
       (let* ([new-lev (+ lev 1)]
              [ret1 (resolve-decl-list env new-lev parms)]
              [new-parms (car ret1)]
              [parms-env (cdr ret1)]
              [new-body (resolve-stmt parms-env new-lev body)]
              [ret2 (env name)])
         (if ret2
             (let ([kind (decl-kind ret2)]
                   [type (decl-type ret2)])
               (if (or
                     (eq? kind 'var)
                     (eq? kind 'fun)
                     (and
                       (eq? kind 'proto)
                       (not (equal? (list* 'fun ret-ty (map stx:parm-decl-ty parms)) type))))
                   (redef-err pos name)
                   (register-fun env name ret-ty new-parms new-body pos)))
            (register-fun env name ret-ty new-parms new-body pos)))]
      [(stx:parm-decl name ty pos)
       (let ([ret (env name)])
         (if ret
             (let ([kind (decl-kind ret)])
               (if (eq? kind 'parm)
                 (redef-err pos name)
                 (register-parm env name ty pos)))
             (register-parm env name ty pos)))]))
  (define (resolve-stmt-list env lev stmt-list)
    (map (lambda (stmt)
           (resolve-stmt env lev stmt))
         stmt-list))
  (define (resolve-stmt env lev stmt)
    (match stmt
      ['() stmt]
      [(cons _ _) (resolve-exp-list env lev stmt)]
      [(stx:cmpd-stmt decls stmts pos)
       (let* ([new-lev (+ lev 1)]
              [ret1 (resolve-decl-list env new-lev decls)]
              [new-decls (car ret1)]
              [new-env (cdr ret1)]
              [new-stmts (resolve-stmt-list new-env new-lev stmts)])
         (stx:cmpd-stmt new-decls new-stmts pos))]
      [(stx:if-els-stmt test tbody ebody pos)
       (let ([new-test (resolve-exp-list env lev test)]
             [new-tbody (resolve-stmt env lev tbody)]
             [new-ebody (resolve-stmt env lev ebody)])
         (stx:if-els-stmt new-test new-tbody new-ebody pos))]
      [(stx:while-stmt test body pos)
       (let ([new-test (resolve-exp-list env lev test)]
             [new-body (resolve-stmt env lev body)])
         (stx:while-stmt new-test new-body pos))]
      [(stx:ret-stmt exp pos)
       (let ([new-exp (resolve-exp-list env lev exp)])
         (stx:ret-stmt new-exp pos))]))
  (define (resolve-exp-list env lev exp-list)
    (map (lambda (exp)
           (resolve-exp env lev exp))
         exp-list))
  (define (resolve-exp env lev exp)
    (match exp
      ['() '()]
      [(cons _ _) (resolve-exp-list env lev exp)]
      [(stx:assign-exp left right pos)
       (let ([new-left (resolve-exp env lev left)]
             [new-right (resolve-exp env lev right)])
         (stx:assign-exp new-left new-right pos))]
      [(stx:lop-exp op left right pos)
       (let ([new-left (resolve-exp env lev left)]
             [new-right (resolve-exp env lev right)])
         (stx:lop-exp op new-left new-right pos))]
      [(stx:rop-exp op left right pos)
       (let ([new-left (resolve-exp env lev left)]
             [new-right (resolve-exp env lev right)])
         (stx:rop-exp op new-left new-right pos))]
      [(stx:aop-exp op left right pos)
       (let ([new-left (resolve-exp env lev left)]
             [new-right (resolve-exp env lev right)])
         (stx:aop-exp op new-left new-right pos))]
      [(stx:addr-exp var pos)
       (let ([new-var (resolve-exp env lev var)])
         (stx:addr-exp new-var pos))]
      [(stx:deref-exp arg pos)
       (let ([new-arg (resolve-exp env lev arg)])
         (stx:deref-exp new-arg pos))]
      [(stx:fun-exp name args pos)
       (let ([ret (env name)])
         (if ret
             (let ([kind (decl-kind ret)])
               (if (or (eq? kind 'var) (eq? kind 'parm))
                   (error 'name-resolve-error (err-msg pos (format "~a is not a function" name)))
                   (stx:fun-exp ret args pos)))
             (unknown-err pos name)))]
      [(stx:var-exp tgt pos)
       (let ([ret (env tgt)])
         (if ret
             (let ([kind (decl-kind ret)])
               (if (or (eq? kind 'fun) (eq? kind 'proto))
                   (error 'name-resolve-error (err-msg pos (format "~a is not a variable" tgt)))
                   (stx:var-exp ret pos)))
             (unknown-err pos tgt)))]
      [else exp]))
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
  (define (redef-warn pos name)
    (eprintf (err-msg pos (format "warning: overwriting of parm ~a" name))))
  (define (redef-err pos name)
    (error 'name-resolve-error (err-msg pos (format "redifinition of ~a" name))))
  (define (unknown-err pos name)
    (error 'name-resolve-error (err-msg pos (format "unknown identifier ~a" name))))
  (car (resolve-decl-list initial-env 0 ast)))

(define (name-resolve-str str)
  (name-resolve (parse-string str)))
