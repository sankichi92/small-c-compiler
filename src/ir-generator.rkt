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
        [(stx:fun-decl obj ret-ty parm-tys pos) (ir:fun-def obj '() '())]
        [(stx:fun-def obj ret-ty parms body pos)
         (let ([new-parms (map decl->ir parms)]
               [new-body (car (stmt->ir body var-maxid))])
           (ir:fun-def obj new-parms new-body))]))
    (define (stmt->ir stmt)
      (match stmt
        ['() '()]
        [(cons _ _)
         (let ([dest (fresh-obj)])
           (exp->ir dest stmt))]
        [(stx:cmpd-stmt decls stmts pos)
         (list (ir:cmpd-stmt decls stmts))]
        [(stx:if-els-stmt test tbody ebody pos)
         (let ([test-var (fresh-obj)]
               [label1 (fresh-label)]
               [label2 (fresh-label)]
               [label3 (fresh-label)])
           `(,@(exp->ir test-var test)
             ,(ir:if-stmt test-var
                          (ir:goto-stmt label1)
                          (ir:goto-stmt label2))
             ,(ir:label-stmt label1)
             ,@(stmt->ir tbody)
             ,(ir:goto-stmt label3)
             ,(ir:label-stmt label2)
             ,@(stmt->ir ebody)
             ,(ir:label-stmt label3)))]
        [(stx:while-stmt test body pos)
         (let ([test-var (fresh-obj)]
               [label1 (fresh-label)]
               [label2 (fresh-label)]
               [label3 (fresh-label)])
           `(,(ir:label-stmt label1)
             ,@(exp->ir test-var test)
             ,(ir:if-stmt test-var
                          (ir:goto-stmt label2)
                          (ir:goto-stmt label3))
             ,(ir:label-stmt label2)
             ,@(stmt->ir body)
             ,(ir:goto-stmt label1)
             ,(ir:label-stmt label3)))]
        [(stx:ret-stmt exp pos)
         (let ([ret-var (fresh-obj)])
           `(,@(exp->ir ret-var exp)
             ,(ir:ret-stmt ret-var)))]))
    (define (exp->ir dest exp)
      (match exp
        ['() '()]
        [(cons _ _)
         (append-map (lambda (e)
                             (exp->ir dest e))
                     exp)]
        [(stx:assign-exp left right pos)
         (let ([left-var (fresh-obj)]
               [right-var (fresh-obj)])
           `(,@(exp->ir left-var left)
             ,@(exp->ir right-var right)
             ,(ir:assign-stmt left-var (ir:var-exp right-var))
             ,(ir:assign-stmt dest (ir:var-exp right-var))))]
        [(stx:lop-exp op left right pos)
         (let* ([left-var (fresh-obj)]
                [right-var (fresh-obj)]
                [label1 (fresh-label)]
                [label2 (fresh-label)]
                [label3 (fresh-label)]
                [label4 (fresh-label)]
                [left-ir (exp->ir left-var left)]
                [right-ir (exp->ir right-var right)])
           (match op
             ['|| `(,@left-ir
                    ,@right-ir
                    ,(ir:if-stmt left-var
                                 (ir:goto-stmt label1)
                                 (ir:goto-stmt label2))
                    ,(ir:label-stmt label1)
                    ,(ir:assign-stmt dest (ir:lit-exp 1))
                    ,(ir:goto-stmt label3)
                    ,(ir:label-stmt label2)
                    ,(ir:if-stmt right-var
                                 (ir:goto-stmt label1)
                                 (ir:goto-stmt label4))
                    ,(ir:label-stmt label4)
                    ,(ir:assign-stmt dest (ir:lit-exp 0))
                    ,(ir:label-stmt label3))]
             ['&& `(,@left-ir
                    ,@right-ir
                    ,(ir:if-stmt left-var
                                 (ir:goto-stmt label1)
                                 (ir:goto-stmt label2))
                    ,(ir:label-stmt label1)
                    ,(ir:if-stmt right-var
                                 (ir:goto-stmt label3)
                                 (ir:goto-stmt label2))
                    ,(ir:label-stmt label3)
                    ,(ir:assign-stmt dest (ir:lit-exp 1))
                    ,(ir:goto-stmt label4)
                    ,(ir:label-stmt label2)
                    ,(ir:assign-stmt dest (ir:lit-exp 0))
                    ,(ir:label-stmt label4))]))]
        [(stx:rop-exp op left right pos)
         (let ([left-var (fresh-obj)]
               [right-var (fresh-obj)])
           `(,@(exp->ir left-var left)
             ,@(exp->ir right-var right)
             ,(ir:assign-stmt dest (ir:rop-exp op left-var right-var))))]
        [(stx:aop-exp op left right pos)
         (let ([left-var (fresh-obj)]
               [right-var (fresh-obj)])
           `(,@(exp->ir left-var left)
             ,@(exp->ir right-var right)
             ,(ir:assign-stmt dest (ir:aop-exp op left-var right-var))))]
        [(stx:addr-exp var pos)
         (let ([src (fresh-obj)])
           `(,@(exp->ir src var)
             ,(ir:write-stmt dest src)))]
        [(stx:deref-exp arg pos)
         (let ([src (fresh-obj)])
           `(,@(exp->ir src arg)
             ,(ir:read-stmt dest src)))]
        [(stx:fun-exp obj args pos)
         (let ([vars (append-map (lambda (e)
                                         (ir:var-exp (exp->ir (fresh-obj) e)))
                                 args)])
           (list (ir:call-stmt dest obj vars)))]
        [(stx:var-exp obj pos)
         (list (ir:assign-stmt dest (ir:var-exp obj)))]
        [(stx:lit-exp val pos)
         (list (ir:assign-stmt dest (ir:lit-exp val)))]))
    (map decl->ir ast)))

(define (string->ir str)
  (let ([ast (cdr (type-check-str str))])
    (ast->ir ast)))
