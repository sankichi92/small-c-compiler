#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "utils.rkt"
         "type-checker.rkt")
(provide ast->ir string->ir)

(define (ast->ir ast)
  (let ([var-maxid 0]
        [label-maxid 0])
    (define (fresh-obj)
      (let* ([oldid var-maxid]
             [sym-str (string-append "_x" (number->string oldid))]
             [sym (string->symbol sym-str)])
        (set! var-maxid (add1 var-maxid))
        (ett:decl sym '() 'temp 'temp)))
    (define (fresh-label)
      (let ([oldid label-maxid])
        (set! label-maxid (+ label-maxid 1))
        (string-append "label" (number->string oldid))))
    (define (decl->ir decl)
      (match decl
        [(stx:var-decl obj ty pos) (ir:var-decl obj)]
        [(stx:parm-decl obj ty pos) (ir:var-decl obj)]
        [(stx:fun-decl obj ret-ty parm-tys pos) (ir:var-decl obj)]
        [(stx:fun-def obj ret-ty parms body pos)
         (let ([new-parms (append-map decl->ir parms)]
               [new-body (car (stmt->ir body))])
           (ir:fun-def obj new-parms new-body))]))
    (define (stmt->ir stmt)
      (match stmt
        ['() stmt]
        [(cons _ _) stmt]
        [(stx:if-els-stmt test tbody ebody pos) stmt]
        [(stx:cmpd-stmt decls stmts pos)
         (list (ir:cmpd-stmt decls stmts))]
        [(stx:while-stmt test body pos) stmt]
        [(stx:ret-stmt exp pos) stmt]
        [else stmt]))
    (define (exp->ir exp)
      (match exp
        ['() exp]
        [(cons _ _) exp]
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
    (map decl->ir ast)))

(define (string->ir str)
  (let ([ast (cdr (type-check-str str))])
    (ast->ir ast)))
