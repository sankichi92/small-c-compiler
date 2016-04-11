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
      [(stx:parm-decl obj ty pos) (check-type-obj obj pos)]
      [(stx:fun-decl obj ret-ty parm-tys pos) (check-type-obj obj pos)]
      [(stx:fun-def obj ret-ty parms body pos)
       (if (and (well-typed? (check-type-obj obj pos))
                (well-typed? (type-check-stmt body ret-ty))
           'well-typed
           (tc-err pos "fun-def is not well-typed"))]))
  (define (type-check-stmt stmt . ret-ty)
    (match stmt
      ['() 'well-typed]
      [id 'well-typed]
      [(stx:if-els-stmt test tbody ebody pos)
       (if (and (int? test)
                (well-typed? tbody)
                (well-typed? ebody))
           'well-typed
           (tc-err pos "if-els-stmt is not well-typed"))]
      [(stx:while-stmt test body pos)
       (if (and (int? test)
                (well-typed? body))
           'well-typed
           (tc-err pos "while-stmt is not well-typed"))]
      [(stx:ret-stmt exp pos)
       (if (null? ret-ty)
           stmt
           (if (eq? ret-ty 'void)
               (if (null? exp)
                   'well-typed
                   (tc-err pos "void function should not return a value"))
               (if (eq? exp ret-ty)
                   'well-typed
                   (tc-err pos "non-void function should return a value"))))]
      [(stx:cmpd-stmt decls stmts pos)
       (if (and (andmap well-typed? decls)
                (andmap well-typed? stmts))
           'well-typed
           (tc-err pos "cmpd-stmt is not well-typed"))]
      ['well-typed 'well-typed]))
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
    (let* ([type (ett:decl-type obj)])
      (if (match type
            ['void (tc-err pos "variable has incomplete type 'void'")]
            [(list 'array 'void _) (tc-err pos "array has incomplete element type 'void'")]
            [(list 'pointer 'void) (tc-err pos "pointer has incomplete type 'void'")]
            [(list 'array (list 'pointer 'void) _) (tc-err pos "array has incomplete element type '*void'")]
            [(list 'fun (cons ret-ty args)) (andmap (lambda (a)
                                                      (check-type-obj a pos))
                                                    args)]
            [(list 'array t _) (check-type-obj t)]
            [(list 'pointer t) (check-type-obj t)]
            [else #t])
          'well-typed
          (tc-err pos "object is not well-typed"))))
  (define (int? exp)
    (eq? exp 'int))
  (define (tc-err pos msg)
    (error 'type-check-error (err-msg pos msg)))
  (let ([new-ast (traverse type-check-decl type-check-stmt type-check-exp ast)])
    (if (andmap well-typed? new-ast)
        'well-typed
        (tc-err (position 1 1 0) "program is not well-typed"))))

(define (type-check-str str)
  (type-check (deference-check-str str)))
