#lang racket
(require (prefix-in stx: "syntax.rkt"))
(provide traverse)

(define (traverse decl-proc stmt-proc exp-proc ast [map map])
  (define (traverse-decl decl)
    (let ([new-decl
           (match decl
             [(stx:fun-def name ret-ty parms body pos)
              (let ([new-parms (map traverse-decl parms)]
                    [new-body (traverse-stmt body)])
                (stx:fun-def name ret-ty new-parms new-body pos))]
             [else decl])])
      (decl-proc new-decl)))
  (define (traverse-stmt stmt)
    (let ([new-stmt
           (match stmt
             ['() '()]
             [(cons _ _) (traverse-exp stmt)]
             [(stx:cmpd-stmt decls stmts pos)
              (let ([new-decls (map traverse-decl decls)]
                    [new-stmts (map traverse-stmt stmts)])
                (stx:cmpd-stmt new-decls new-stmts pos))]
             [(stx:if-els-stmt test tbody ebody pos)
              (let ([new-test (traverse-exp test)]
                    [new-tbody (traverse-stmt tbody)]
                    [new-ebody (traverse-stmt ebody)])
                (stx:if-els-stmt new-test new-tbody new-ebody pos))]
             [(stx:while-stmt test body pos)
              (let ([new-test (traverse-exp test)]
                    [new-body (traverse-stmt body)])
                (stx:while-stmt new-test new-body pos))]
             [(stx:ret-stmt exp pos)
              (let ([new-exp (traverse-exp exp)])
                (stx:ret-stmt new-exp pos))])])
      (stmt-proc new-stmt)))
  (define (traverse-exp exp)
    (let ([new-exp
           (match exp
             ['() '()]
             [(cons _ _) (map traverse-exp exp)]
             [(stx:assign-exp left right pos)
              (let ([new-left (traverse-exp left)]
                    [new-right (traverse-exp right)])
                (stx:assign-exp new-left new-right pos))]
             [(stx:lop-exp op left right pos)
              (let ([new-left (traverse-exp left)]
                    [new-right (traverse-exp right)])
                (stx:lop-exp op new-left new-right pos))]
             [(stx:rop-exp op left right pos)
              (let ([new-left (traverse-exp left)]
                    [new-right (traverse-exp right)])
                (stx:rop-exp op new-left new-right pos))]
             [(stx:aop-exp op left right pos)
              (let ([new-left (traverse-exp left)]
                    [new-right (traverse-exp right)])
                (stx:aop-exp op new-left new-right pos))]
             [(stx:addr-exp var pos)
              (let ([new-var (traverse-exp var)])
                (stx:addr-exp new-var pos))]
             [(stx:deref-exp arg pos)
              (let ([new-arg (traverse-exp arg)])
                (stx:deref-exp new-arg pos))]
             [(stx:fun-exp name args pos)
              (let ([new-args (map traverse-exp args)])
                (stx:fun-exp name new-args pos))]
             [else exp])])
      (exp-proc new-exp)))
  (map traverse-decl ast))

;; template

(define (foo ast)
  (define (foo-decl decl)
    (match decl
      [(stx:var-decl name ty pos) decl]
      [(stx:parm-decl name ty pos) decl]
      [(stx:fun-decl name ret-ty parm-tys pos) decl]
      [(stx:fun-def name ret-ty parms body pos) decl]
      [else decl]))
  (define (foo-stmt stmt)
    (match stmt
      ['() stmt]
      [(cons _ _) stmt]
      [(stx:if-els-stmt test tbody ebody pos) stmt]
      [(stx:while-stmt test body pos) stmt]
      [(stx:ret-stmt exp pos) stmt]
      [(stx:cmpd-stmt decls stmts pos) stmt]
      [else stmt]))
  (define (foo-exp exp)
    (match exp
      ['() exp]
      [(cons _ _) exp]
      [(stx:assign-exp left right pos) exp]
      [(stx:lop-exp op left right pos) exp]
      [(stx:rop-exp op left right pos) exp]
      [(stx:aop-exp op left right pos) exp]
      [(stx:addr-exp var pos) exp]
      [(stx:deref-exp arg pos) exp]
      [(stx:fun-exp name args pos) exp]
      [(stx:var-exp tgt pos) exp]
      [(stx:lit-exp val pos) exp]
      [else exp]))
  (traverse foo-decl foo-stmt foo-exp ast))
