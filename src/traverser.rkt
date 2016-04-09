#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt"))
(provide (all-defined-out))

(define (traverser decl-proc stmt-proc exp-proc ast)
  (define (traverse-decl decl)
    (let ([new-decl
           (cond [(stx:fun-def? decl)
                  (let* ([name (stx:fun-def-name decl)]
                         [ret-ty (stx:fun-def-ret-ty decl)]
                         [parms (stx:fun-def-parms decl)]
                         [body (stx:fun-def-body decl)]
                         [pos (stx:fun-def-pos decl)]
                         [new-params (map traverse-decl parms)]
                         [new-body (traverse-stmt body)])
                    (stx:fun-def name ret-ty new-params new-body pos))]
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
                [(stx:if-els-stmt? stmt) stmt]
                [(stx:while-stmt? stmt) stmt]
                [(stx:ret-stmt? stmt) stmt])])
      (stmt-proc new-stmt)))
  (define (traverse-exp exp) exp)
  (map traverse-decl ast))
