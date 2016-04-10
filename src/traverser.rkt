#lang racket
(require (prefix-in stx: "syntax.rkt"))
(provide traverse)

(define (traverse decl-proc stmt-proc exp-proc ast)
  (define (traverse-decl decl)
    (let ([new-decl
           (cond [(stx:fun-def? decl)
                  (let* ([name (stx:fun-def-name decl)]
                         [ret-ty (stx:fun-def-ret-ty decl)]
                         [parms (stx:fun-def-parms decl)]
                         [body (stx:fun-def-body decl)]
                         [pos (stx:fun-def-pos decl)]
                         [new-parms (map traverse-decl parms)]
                         [new-body (traverse-stmt body)])
                    (stx:fun-def name ret-ty new-parms new-body pos))]
                 [else decl])])
      (decl-proc new-decl)))
  (define (traverse-stmt stmt)
    (let ([new-stmt
          (cond [(null? stmt) '()]
                [(list? stmt) (map traverse-exp stmt)]
                [(stx:cmpd-stmt? stmt)
                 (let* ([decls (stx:cmpd-stmt-decls stmt)]
                        [stmts (stx:cmpd-stmt-stmts stmt)]
                        [pos (stx:cmpd-stmt-pos stmt)]
                        [new-decls (map traverse-decl decls)]
                        [new-stmts (map traverse-stmt stmts)])
                   (stx:cmpd-stmt new-decls new-stmts pos))]
                [(stx:if-els-stmt? stmt)
                 (let* ([test (stx:if-els-stmt-test stmt)]
                        [tbody (stx:if-els-stmt-tbody stmt)]
                        [ebody (stx:if-els-stmt-ebody stmt)]
                        [pos (stx:if-els-stmt-pos stmt)]
                        [new-test (map traverse-exp test)]
                        [new-tbody (map traverse-stmt tbody)]
                        [new-ebody (map traverse-stmt ebody)])
                   (stx:if-els-stmt new-test new-tbody new-ebody pos))]
                [(stx:while-stmt? stmt)
                 (let* ([test (stx:while-stmt-test stmt)]
                        [body (stx:while-stmt-body stmt)]
                        [pos (stx:while-stmt-pos stmt)]
                        [new-test (map traverse-exp test)]
                        [new-body (map traverse-stmt body)])
                    (stx:while-stmt new-test new-body pos))]
                [(stx:ret-stmt? stmt)
                 (let* ([exp (stx:ret-stmt-exp stmt)]
                        [pos (stx:ret-stmt-pos stmt)]
                        [new-exp (map traverse-exp exp)])
                   (stx:ret-stmt new-exp pos))])])
      (stmt-proc new-stmt)))
  (define (traverse-exp exp)
    (let ([new-exp
          (cond [(null? exp) '()]
                [(list? exp) (map traverse-exp exp)]
                [(stx:assign-exp? exp)
                 (let* ([left (stx:assign-exp-left exp)]
                        [right (stx:assign-exp-right exp)]
                        [pos (stx:assign-exp-pos exp)]
                        [new-left (traverse-exp left)]
                        [new-right (traverse-exp right)])
                   (stx:assign-exp new-left new-right pos))]
                [(stx:lop-exp? exp)
                 (let* ([op (stx:lop-exp-op exp)]
                        [left (stx:lop-exp-left exp)]
                        [right (stx:lop-exp-right exp)]
                        [pos (stx:lop-exp-pos exp)]
                        [new-left (traverse-exp left)]
                        [new-right (traverse-exp right)])
                   (stx:lop-exp op new-left new-right pos))]
                [(stx:rop-exp? exp)
                 (let* ([op (stx:rop-exp-op exp)]
                        [left (stx:rop-exp-left exp)]
                        [right (stx:rop-exp-right exp)]
                        [pos (stx:rop-exp-pos exp)]
                        [new-left (traverse-exp left)]
                        [new-right (traverse-exp right)])
                   (stx:rop-exp op new-left new-right pos))]
                [(stx:aop-exp? exp)
                 (let* ([op (stx:aop-exp-op exp)]
                        [left (stx:aop-exp-left exp)]
                        [right (stx:aop-exp-right exp)]
                        [pos (stx:aop-exp-pos exp)]
                        [new-left (traverse-exp left)]
                        [new-right (traverse-exp right)])
                   (stx:aop-exp op new-left new-right pos))]
                [(stx:addr-exp? exp)
                 (let* ([var (stx:addr-exp-var exp)]
                        [pos (stx:addr-exp-pos exp)]
                        [new-var (traverse-exp var)])
                   (stx:addr-exp new-var pos))]
                [(stx:deref-exp? exp)
                 (let* ([arg (stx:deref-exp-arg exp)]
                        [pos (stx:deref-exp-pos exp)]
                        [new-arg (traverse-exp arg)])
                   (stx:deref-exp new-arg pos))]
                [(stx:fun-exp? exp)
                 (let* ([name (stx:fun-exp-name exp)]
                        [args (stx:fun-exp-args exp)]
                        [pos (stx:fun-exp-pos)]
                        [new-args (map traverse-exp args)])
                   (stx:fun-exp name new-args pos))]
                [else exp])])
      (exp-proc new-exp)))
  (map traverse-decl ast))

;; template

(define (foo ast)
  (define (foo-decl decl)
    (cond [(stx:var-decl? decl) decl]
          [(stx:parm-decl? decl) decl]
          [(stx:fun-decl? decl) decl]
          [(stx:fun-def? decl) decl]
          [else decl]))
  (define (foo-stmt stmt)
    (cond [(stx:if-els-stmt? stmt) stmt]
          [(stx:while-stmt? stmt) stmt]
          [(stx:ret-stmt? stmt) stmt]
          [(stx:cmpd-stmt? stmt) stmt]
          [else stmt]))
  (define (foo-exp exp)
    (cond [(stx:assign-exp? exp) exp]
          [(stx:lop-exp? exp) exp]
          [(stx:rop-exp? exp) exp]
          [(stx:aop-exp? exp) exp]
          [(stx:addr-exp? exp) exp]
          [(stx:deref-exp? exp) exp]
          [(stx:fun-exp? exp) exp]
          [(stx:var-exp exp) exp]
          [(stx:lit-exp exp) exp]
          [else exp]))
  (traverse foo-decl foo-stmt foo-exp ast))
