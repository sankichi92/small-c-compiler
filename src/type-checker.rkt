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
                (well-typed? (type-check-stmt body ret-ty)))
           'well-typed
           decl)]))
  (define (type-check-stmt stmt . ret-ty)
    (match stmt
      ['well-typed 'well-typed]
      ['() 'well-typed]
      [id 'well-typed]
      [(stx:if-els-stmt test tbody ebody pos)
       (if (and (int? test)
                (well-typed? tbody)
                (well-typed? ebody))
           'well-typed
           stmt)]
      [(stx:while-stmt test body pos)
       (if (and (int? test)
                (well-typed? body))
           'well-typed
      [(stx:ret-stmt exp pos)
       (cond [(null? ret-ty) stmt]
             [(and (eq? ret-ty 'void)
                   (not (null? exp)))
              (tc-err pos "void function should not return a value")]
             [(not (eq? ret-ty exp))
              (tc-err pos "non-void function should return a value")]
             [else 'well-typed])]
           stmt)]
      [(stx:cmpd-stmt decls stmts pos)
       (if (and (andmap well-typed? decls)
                (andmap well-typed? stmts))
           'well-typed
           stmt)]
  (define (type-check-exp exp)
    (match exp
      ['() 'well-typed]
      [(cons _ _)
       (if (andmap symbol? exp)
           (list-tail exp 1)
           (tc-err '() "exp-list is not well-typed"))]
      [(stx:assign-exp left right pos)
       (if (eq? left right)
           left
           (tc-err pos (format "incompatible assigning to '~a' from '~a'" left right)))]
      [(stx:lop-exp op left right pos)
       (if (and (int? left)
                (int? right))
           'int
           (tc-err pos (format "invalid operands ('~a' and '~a')" left right)))]
      [(stx:rop-exp op left right pos)
       (if (eq? left right)
           'int
           (tc-err pos (format "comparison between '~a' and '~a'" left right)))]
      [(stx:aop-exp op left right pos)
       (cond [(and (int? left)
                   (int? right))
              'int]
             [(match op
                ['+ (cond [(or (and (int*? left) (int? right))
                               (and (int? left) (int*? right)))
                           'int*]
                          [(or (and (int**? left) (int? right))
                               (and (int? left) (int**? right)))
                           'int**])]
                ['- (cond [(and (int*? left) (int? right))
                           'int*]
                          [(and (int**? left) (int? right))
                           'int**])])]
             [else (tc-err pos (format "invalid operands ('~a' and '~a')" left right))])]
      [(stx:addr-exp var pos)
       (if (int? var)
           'int*
           (tc-err pos (format "dereference requires int operand ('~a' invalid)" var)))]
      [(stx:deref-exp arg pos)
       (cond [(int*? arg) 'int]
             [(int**? arg) 'int*]
             [else (tc-err pos "indirection requires pointer operand ('~a' invalid)" arg)])]
      [(stx:fun-exp obj args pos)
       (let* ([type (ett:decl-type obj)]
              [ret-ty (cadr type)]
              [arg-tys (cddr type)]
              [arg-syms (map type->symbol arg-tys)])
         (if (and (andmap symbol? args)
                  (equal? arg-syms args))
             ret-ty
             (tc-err pos (format "invalid arguments to function call, expected '~a', have '~a'" arg-syms args))))]
      [(stx:var-exp obj pos)
       (type->symbol (ett:decl-type obj))]
      [(stx:lit-exp val pos) 'int]))
  (define (check-type-obj obj pos)
    (define (check-type-obj-ty type pos)
      (match type
        ['void
         (tc-err pos "variable has incomplete type 'void'")]
        [(list 'array 'void _)
         (tc-err pos "array has incomplete element type 'void'")]
        [(list 'pointer 'void)
         (tc-err pos "pointer has incomplete type 'void'")]
        [(list 'array (list 'pointer 'void) _)
         (tc-err pos "array has incomplete element type 'void *'")]
        [(list 'fun (cons ret-ty args))
         (andmap (lambda (a)
                   (check-type-obj-ty a pos))
                 args)]
        [(list 'array t _) (check-type-obj-ty t pos)]
        [(list 'pointer t) (check-type-obj-ty t pos)]
        [else #t]))
    (let* ([type (ett:decl-type obj)])
      (if (check-type-obj-ty type pos)
          'well-typed
          (tc-err pos "invalid type"))))
  (define (type->symbol type)
    (define (type->string type)
      (match type
        ['int "int"]
        [(list 'pointer ty)
         (string-join (type->string ty) "*")]
        [(list 'array ty _)
         (string-join (type->string ty) "*")]))
    (string->symbol (type->string type)))
  (define (int? exp)
    (eq? exp 'int))
  (define (int*? exp)
    (eq? exp 'int*))
  (define (int**? exp)
    (eq? exp 'int**))
  (define (tc-err pos msg)
    (error 'type-check-error (err-msg pos msg)))
  (let ([decls (traverse type-check-decl type-check-stmt type-check-exp ast)])
    (if (andmap well-typed? decls)
        'well-typed
        decls)))

(define (type-check-str str)
  (type-check (deference-check-str str)))
