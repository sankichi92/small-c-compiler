lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "utils.rkt"
         "traverser.rkt"
         "type-checker.rkt")
(provide ast->ir)

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
        [(stx:var-decl name ty pos) decl]
        [(stx:parm-decl name ty pos) decl]
        [(stx:fun-decl name ret-ty parm-tys pos) decl]
        [(stx:fun-def name ret-ty parms body pos) decl]
        [else decl]))
    (define (stmt->ir stmt)
      (match stmt
        ['() stmt]
        [(cons _ _) stmt]
        [(stx:if-els-stmt test tbody ebody pos) stmt]
        [(stx:while-stmt test body pos) stmt]
        [(stx:ret-stmt exp pos) stmt]
        [(stx:cmpd-stmt decls stmts pos) stmt]
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
        [(stx:fun-exp name args pos) exp]
        [(stx:var-exp tgt pos) exp]
        [(stx:lit-exp val pos) exp]
        [else exp]))
    (traverse decl->ir stmt->ir exp->ir ast)))
