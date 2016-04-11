#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt"
         "traverser.rkt"
         "deference-checker.rkt")
(provide well-typed? type-check)

(define (well-typed? sym)
  (eq? sym 'well-typed))

(define (type-check ast)
  (define (type-check-decl decl)
    (match decl
      [(stx:var-decl name ty pos) decl]
      [(stx:parm-decl name ty pos) decl]
      [(stx:fun-decl name ret-ty parm-tys pos) decl]
      [(stx:fun-def name ret-ty parms body pos) decl]
      [else decl]))
  (define (type-check-stmt stmt)
    (match stmt
      [(stx:if-els-stmt test tbody ebody pos) stmt]
      [(stx:while-stmt test body pos) stmt]
      [(stx:ret-stmt exp pos) stmt]
      [(stx:cmpd-stmt decls stmts pos) stmt]
      [else stmt]))
  (define (type-check-exp exp)
    (match exp
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
  (define (tc-err pos msg)
    (error 'type-check-error (err-msg pos msg)))
  (if (andmap well-typed?
              (traverse type-check-decl type-check-stmt type-check-exp ast))
      'well-typed
      (tc-err (position 1 1 0) "program is not well-typed")))
