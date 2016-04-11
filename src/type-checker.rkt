#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt"
         "traverser.rkt"
         "deference-checker.rkt")
(provide well-typed? type-check type-check-str)

(define (well-typed? sym)
  (eq? sym 'well-typed))

(define (type-check ast)
  (define (type-check-decl decl)
    (match decl
      [(stx:var-decl obj ty pos) (check-type-obj obj pos)]
      [(stx:parm-decl obj ty pos) decl]
      [(stx:fun-decl obj ret-ty parm-tys pos) decl]
      [(stx:fun-def obj ret-ty parms body pos) decl]
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
      [(stx:fun-exp obj args pos) exp]
      [(stx:var-exp obj pos) exp]
      [(stx:lit-exp val pos) exp]
      [else exp]))
  (define (check-type-obj obj pos)
    (let ([type (ett:decl-type obj)])
      (match type
        [`(array void ,_) (tc-err pos "array has incomplete element type 'void'")]
        [else 'well-typed])))
  (define (tc-err pos msg)
    (error 'type-check-error (err-msg pos msg)))
  (if (andmap well-typed?
              (traverse type-check-decl type-check-stmt type-check-exp ast))
      'well-typed
      #f))

(define (type-check-str str)
  (type-check (deference-check-str str)))
