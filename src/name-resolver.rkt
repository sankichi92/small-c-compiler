#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt")
(provide name-resolve)

(define (name-resolve ast)
  (define initial-env (lambda (x) #f))
  (define (register env decl)
    (lambda (name)
      (if (eq? name (ett:decl-name decl))
          decl
          (env name))))
  (define (resolve-decl-list env lev decl-list)
    (foldl (lambda (decl acc)
             (let* ([ret (resolve-decl (cdr acc) lev decl)]
                    [new-decl (car ret)]
                    [new-env (cdr ret)])
               (cons (append (car acc) (list new-decl))
                     new-env)))
           (cons '() env)
           decl-list))
  (define (resolve-decl env lev decl)
    (define (redef-err pos name)
      (resolve-err pos (format "redifinition of '~a'" name)))
    (match decl
      [(stx:var-decl name ty pos)
       (let ([obj (env name)])
         (if (and obj
                  (let ([kind (ett:decl-kind obj)]
                        [obj-lev (ett:decl-lev obj)])
                    (or
                      (and (or (eq? kind 'fun)
                               (eq? kind 'proto))
                           (= lev 0))
                      (and (eq? kind 'var)
                           (= obj-lev lev))
                      (begin
                        (when (eq? kind 'parm)
                              (eprintf
                                (err-msg pos "warning: overwriting an argument")))
                        #f))))
             (redef-err pos name)
             (let* ([new-obj (ett:decl name lev 'var ty)]
                    [new-env (register env new-obj)])
               (cons (stx:var-decl new-obj ty pos) new-env))))]
      [(stx:fun-decl name ret-ty parm-tys pos)
       (let ([obj (env name)])
         (if (and obj
                  (let ([kind (ett:decl-kind obj)]
                        [type (ett:decl-type obj)])
                    (or
                      (eq? kind 'var)
                      (and
                        (or (eq? kind 'proto) (eq? kind 'fun))
                        (not (equal? (list* 'fun ret-ty parm-tys) type))))))
             (redef-err pos name)
             (let* ([new-obj (ett:decl name 0 'proto (list* 'fun ret-ty parm-tys))]
                    [new-env (register env new-obj)])
               (cons (stx:fun-decl new-obj ret-ty parm-tys pos) new-env))))]
      [(stx:fun-def name ret-ty parms body pos)
       (let ([obj (env name)])
         (if (and obj
                  (let ([kind (ett:decl-kind obj)]
                       [type (ett:decl-type obj)])
                    (or
                      (eq? kind 'var)
                      (eq? kind 'fun)
                      (and
                        (eq? kind 'proto)
                        (not (equal? (list* 'fun ret-ty (map stx:parm-decl-ty parms)) type))))))
             (redef-err pos name)
             (let* ([parm-tys (map stx:parm-decl-ty parms)]
                    [type (list* 'fun ret-ty parm-tys)]
                    [new-obj (ett:decl name 0 'fun type)]
                    [new-env (register env new-obj)]
                    [new-lev (add1 lev)]
                    [ret (resolve-decl-list new-env new-lev parms)]
                    [new-parms (car ret)]
                    [parms-env (cdr ret)]
                    [new-body (resolve-stmt parms-env new-lev body)])
               (cons (stx:fun-def new-obj ret-ty new-parms new-body pos) new-env))))]
      [(stx:parm-decl name ty pos)
       (let ([obj (env name)])
         (if (and obj
                  (let ([kind (ett:decl-kind obj)])
                    (eq? kind 'parm)))
             (redef-err pos name)
             (let* ([new-obj (ett:decl name 1 'parm ty)]
                    [new-env (register env new-obj)])
               (cons (stx:parm-decl new-obj ty pos) new-env))))]))
  (define (resolve-stmt-list env lev stmt-list)
    (map (lambda (stmt)
           (resolve-stmt env lev stmt))
         stmt-list))
  (define (resolve-stmt env lev stmt)
    (match stmt
      ['() stmt]
      [(cons _ _) (resolve-exp-list env lev stmt)]
      [(stx:cmpd-stmt decls stmts pos)
       (let* ([new-lev (add1 lev)]
              [ret (resolve-decl-list env new-lev decls)]
              [new-decls (car ret)]
              [new-env (cdr ret)]
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
    (define (unknown-err pos name)
      (resolve-err pos (format "unknown identifier '~a'" name)))
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
       (let ([new-args (resolve-exp-list env lev args)]
             [obj (env name)])
         (if obj
             (let ([kind (ett:decl-kind obj)]
                   [type (ett:decl-type obj)])
               (if (or (eq? kind 'var) (eq? kind 'parm))
                   (resolve-err pos (format "called object type '~a' is not a function or function pointer" type))
                   (stx:fun-exp obj new-args pos)))
             (unknown-err pos name)))]
      [(stx:var-exp tgt pos)
       (let ([obj (env tgt)])
         (if obj
             (let ([kind (ett:decl-kind obj)]
                   [type (ett:decl-type obj)])
               (if (or (eq? kind 'fun) (eq? kind 'proto))
                   (resolve-err pos (format "called object type '~a' is not a variable or pointer" type))
                   (stx:var-exp obj pos)))
             (unknown-err pos tgt)))]
      [else exp]))
  (define (resolve-err pos msg)
    (error '|name resolve error| (err-msg pos msg)))
  (car (resolve-decl-list initial-env 0 ast)))
