#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt"
         "traverser.rkt"
         "name-resolver.rkt")
(provide deference-check deference-check-str)

(define (deference-check ast)
  (define (check-decl decl) decl)
  (define (check-stmt stmt) stmt)
  (define (check-exp exp)
    (match exp
      [(stx:assign-exp left right pos)
       (if (or (stx:deref-exp? left)
                (and (stx:var-exp? left)
                     (let* ([decl (stx:var-exp-tgt left)]
                            [type (ett:decl-type decl)]
                            [array? (if (eq? type 'int)
                                      #f
                                      (eq? (car type) 'array))])
                        (not array?))))
           exp
           (dc-err pos "="))]
      [(stx:addr-exp var pos)
       (if (stx:var-exp? var)
           exp
           (dc-err pos "&"))]
      [else exp]))
  (define (dc-err pos op)
    (error
      'deference-check-error
      (err-msg pos (format "operator '~a' has only a variable, a pointer or an array as operand" op))))
  (traverse check-decl check-stmt check-exp ast))

(define (deference-check-str str)
  (deference-check (name-resolve-str str)))
