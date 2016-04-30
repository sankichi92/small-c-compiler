#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt"))
(provide ast->ir)

(define (ast->ir checked-ast)
  (let ([var-maxid 0]
        [label-maxid 0]
        [temp-objs (lambda (id) '())])
    (define (id->name id)
      (let* ([id-str (number->string id)]
             [name-str (string-append "_x" id-str)])
        (string->symbol name-str)))
    (define (fresh-obj)
      (let* ([old-objs temp-objs]
             [name (id->name var-maxid)]
             [decl (ett:decl name '() 'temp 'temp)])
        (set! var-maxid (add1 var-maxid))
        (set! temp-objs
          (lambda (id)
            (if (eq? (id->name id) (ett:decl-name decl))
                (list decl)
                `(,@(old-objs id) ,decl))))
        decl))
    (define (fresh-label)
      (let ([oldid label-maxid])
        (set! label-maxid (+ label-maxid 1))
        (string-append "L" (number->string oldid))))
    (define (decl->ir decl)
      (match decl
        [(stx:var-decl obj ty pos)
         (list (ir:var-decl obj))]
        [(stx:parm-decl obj ty pos)
         (list (ir:var-decl obj))]
        [(stx:fun-decl obj ret-ty parm-tys pos)
         '()]
        [(stx:fun-def obj ret-ty parms body pos)
         (let ([new-parms (append-map decl->ir parms)]
               [new-body (car (stmt->ir body var-maxid))])
           (list (ir:fun-def obj new-parms new-body)))]))
    (define (stmt->ir stmt [var-id '()])
      (match stmt
        ['() '()]
        [(cons _ _)
         (let ([dest (fresh-obj)])
           (exp->ir dest stmt))]
        [(stx:cmpd-stmt decls stmts pos)
         (let* ([new-decls (append-map decl->ir decls)]
                [new-stmts (append-map stmt->ir stmts)]
                [temp-decls (if (null? var-id)
                                '()
                                (map ir:var-decl (temp-objs var-id)))])
           (list (ir:cmpd-stmt (append new-decls temp-decls) new-stmts)))]
        [(stx:if-els-stmt test tbody ebody pos)
         (let ([test-var (fresh-obj)]
               [label1 (fresh-label)]
               [label2 (fresh-label)]
               [label3 (fresh-label)])
           `(,@(exp->ir test-var test)
             ,(ir:if-stmt test-var label1 label2)
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
             ,(ir:if-stmt test-var label2 label3)
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
         (if (stx:deref-exp? left)
             (let ([left-var (fresh-obj)]
                   [right-var (fresh-obj)])
               `(,@(exp->ir left-var (stx:deref-exp-arg left))
                 ,@(exp->ir right-var right)
                 ,(ir:write-stmt left-var right-var)))
             (let ([left-var (stx:var-exp-tgt left)])
               `(,@(exp->ir left-var right))))]
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
                    ,(ir:if-stmt left-var label1 label2)
                    ,(ir:label-stmt label1)
                    ,(ir:assign-stmt dest (ir:lit-exp 1))
                    ,(ir:goto-stmt label3)
                    ,(ir:label-stmt label2)
                    ,(ir:if-stmt right-var label1 label4)
                    ,(ir:label-stmt label4)
                    ,(ir:assign-stmt dest (ir:lit-exp 0))
                    ,(ir:label-stmt label3))]
             ['&& `(,@left-ir
                    ,@right-ir
                    ,(ir:if-stmt left-var label1 label2)
                    ,(ir:label-stmt label1)
                    ,(ir:if-stmt right-var label3 label2)
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
               [right-var (fresh-obj)]
               [offset (fresh-obj)])
           (cond [(and (or (eq? '+ op) (eq? '- op))
                       (stx:var-exp? left)
                       (let ([type (ett:decl-type (stx:var-exp-tgt left))])
                         (and (list? type) (eq? 'array (first type)))))
                  `(,@(exp->ir left-var left)
                    ,@(exp->ir right-var right)
                    ,(ir:assign-stmt offset (ir:lit-exp 4))
                    ,(ir:assign-stmt right-var (ir:aop-exp '* right-var offset))
                    ,(ir:assign-stmt dest (ir:aop-exp op left-var right-var)))]
                 [(and (or (eq? '+ op) (eq? '- op))
                       (stx:var-exp? right)
                       (let ([type (ett:decl-type (stx:var-exp-tgt right))])
                         (and (list? type) (eq? 'array (first type)))))
                  `(,@(exp->ir left-var left)
                    ,@(exp->ir right-var right)
                    ,(ir:assign-stmt offset (ir:lit-exp 4))
                    ,(ir:assign-stmt left-var (ir:aop-exp '* left-var offset))
                    ,(ir:assign-stmt dest (ir:aop-exp op left-var right-var)))]
                 [else
                  `(,@(exp->ir left-var left)
                    ,@(exp->ir right-var right)
                    ,(ir:assign-stmt dest (ir:aop-exp op left-var right-var)))]))]
        [(stx:addr-exp var pos)
         (let ([new-var (fresh-obj)])
           `(,@(exp->ir new-var var)
             ,(ir:assign-stmt dest (ir:addr-exp new-var))))]
        [(stx:deref-exp arg pos)
         (let ([src (fresh-obj)])
           `(,@(exp->ir src arg)
             ,(ir:read-stmt dest src)))]
        [(stx:fun-exp obj args pos)
         (if (eq? 'print (ett:decl-name obj))
             (let ([var (fresh-obj)])
               `(,@(exp->ir var (car args))
                 ,(ir:print-stmt var)))
             (let* ([vars '()]
                    [new-args (append-map
                                  (lambda (e)
                                    (let ([var (fresh-obj)])
                                      (set! vars (append vars (list var)))
                                      (exp->ir var e)))
                                  args)])
               `(,@new-args
                 ,(ir:call-stmt dest obj vars))))]
        [(stx:var-exp obj pos)
         (let ([type (ett:decl-type obj)])
           (if (and (list? type) (eq? (first type) 'array))
               (list (ir:assign-stmt dest (ir:addr-exp obj)))
               (list (ir:assign-stmt dest (ir:var-exp obj)))))]
        [(stx:lit-exp val pos)
         (list (ir:assign-stmt dest (ir:lit-exp val)))]))
    (append-map decl->ir (cdr checked-ast))))
