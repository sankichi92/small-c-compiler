#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt"
         "traverser.rkt")
(provide type-check)

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
  (define (type-check-stmt stmt [ret-ty '()])
    (match stmt
      ['well-typed 'well-typed]
      ['() 'well-typed]
      [(stx:ret-stmt exp pos)
       (if (null? ret-ty)
           stmt
           (let ([ret-sym (type->symbol ret-ty)])
             (cond [(and (eq? ret-sym 'void)
                         (not (null? exp)))
                    (ty-check-err pos "void function should not return a value")]
                   [(not (eq? ret-sym exp))
                    (ty-check-err
                      pos
                      (format "incompatible returning '~a' from a function with result type '~a'" exp ret-sym))]
                   [else 'well-typed])))]
      [(stx:if-els-stmt test tbody ebody pos)
       (if (and (int? test)
                (well-typed? (type-check-stmt tbody ret-ty))
                (well-typed? (type-check-stmt ebody ret-ty)))
           'well-typed
           stmt)]
      [(stx:while-stmt test body pos)
       (if (and (int? test)
                (well-typed? (type-check-stmt body ret-ty)))
           'well-typed
           stmt)]
      [(stx:cmpd-stmt decls stmts pos)
       (if (and (andmap well-typed? decls)
                (andmap well-typed?
                        (map (lambda (stmt)
                               (type-check-stmt stmt ret-ty))
                             stmts)))
           'well-typed
           stmt)]
      [exp (if (symbol? exp)
               'well-typed
               exp)]))
  (define (type-check-exp exp)
    (match exp
      ['() '()]
      [(cons _ _)
       (if (andmap symbol? exp)
           (list-ref exp (sub1 (length exp)))
           (error '|type check error| "exp-list is not well-typed"))]
      [(stx:assign-exp left right pos)
       (if (eq? left right)
           left
           (ty-check-err pos (format "incompatible assigning to '~a' from '~a'" left right)))]
      [(stx:lop-exp op left right pos)
       (if (and (int? left)
                (int? right))
           'int
           (ty-check-err pos (format "invalid operands ('~a' and '~a')" left right)))]
      [(stx:rop-exp op left right pos)
       (if (eq? left right)
           'int
           (ty-check-err pos (format "comparison between '~a' and '~a'" left right)))]
      [(stx:aop-exp op left right pos)
       (cond [(and (int? left)
                   (int? right))
              'int]
             [(or (and (eq? op '+)
                       (or (and (int*? left) (int? right))
                           (and (int? left) (int*? right))))
                  (and (eq? op '-)
                       (int*? left)
                       (int? right)))
              'int*]
             [(or (and (eq? op '+)
                       (or (and (int**? left) (int? right))
                           (and (int? left) (int**? right))))
                  (and (eq? op '-)
                       (int**? left)
                       (int? right)))
              'int**]
             [else
              (ty-check-err pos (format "invalid operands ('~a' and '~a')" left right))])]
      [(stx:addr-exp var pos)
       (if (int? var)
           'int*
           (ty-check-err pos (format "dereference requires int operand ('~a' invalid)" var)))]
      [(stx:deref-exp arg pos)
       (cond [(int*? arg) 'int]
             [(int**? arg) 'int*]
             [else (ty-check-err pos (format "indirection requires pointer operand ('~a' invalid)" arg))])]
      [(stx:fun-exp obj args pos)
       (let* ([type (ett:decl-type obj)]
              [ret-ty (second type)]
              [arg-tys (cddr type)]
              [arg-syms (map type->symbol arg-tys)])
         (if (and (andmap symbol? args)
                  (equal? arg-syms args))
             ret-ty
             (ty-check-err pos (format "invalid arguments to function call, expected '~a', have '~a'" arg-syms args))))]
      [(stx:var-exp obj pos)
       (type->symbol (ett:decl-type obj))]
      [(stx:lit-exp val pos) 'int]))
  (define (check-type-obj obj pos)
    (define (check-type-obj-ty type pos)
      (match type
        [(list 'array 'void _)
         (ty-check-err pos "array has incomplete element type 'void'")]
        [(list 'pointer 'void)
         (ty-check-err pos "pointer has incomplete type 'void'")]
        [(list 'array (list 'pointer 'void) _)
         (ty-check-err pos "array has incomplete element type 'void *'")]
        [(cons 'fun args)
         (andmap (lambda (arg)
                   (check-type-obj-ty arg pos))
                 args)]
        [else #t]))
    (let ([type (ett:decl-type obj)])
      (if (eq? type 'void)
          (ty-check-err pos "variable has incomplete type 'void'")
          (if (check-type-obj-ty type pos)
              'well-typed
              obj))))
  (define (type->symbol type)
    (define (type->string type)
      (match type
        ['int "int"]
        [(list 'pointer ty)
         (string-append (type->string ty) "*")]
        [(list 'array ty _)
         (string-append (type->string ty) "*")]))
    (if (symbol? type)
        type
        (string->symbol (type->string type))))
  (define (well-typed? sym)
    (eq? sym 'well-typed))
  (define (int? exp)
    (eq? exp 'int))
  (define (int*? exp)
    (eq? exp 'int*))
  (define (int**? exp)
    (eq? exp 'int**))
  (define (ty-check-err pos msg)
    (error '|type check error| (err-msg pos msg)))
  (let ([decls (traverse type-check-decl type-check-stmt type-check-exp ast)])
    (if (andmap well-typed? decls)
        (cons 'well-typed ast)
        (cons '() decls))))
