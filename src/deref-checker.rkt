#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         "utils.rkt"
         "traverser.rkt")
(provide dereference-check)

(define (dereference-check ast)
  (define (check-decl decl) decl)
  (define (check-stmt stmt) stmt)
  (define (check-exp exp)
    (match exp
      [(stx:assign-exp left right pos)
       (if (or (stx:deref-exp? left)
               (and (stx:var-exp? left)
                    (let ([type (ett:decl-type (stx:var-exp-tgt left))])
                      (not (array? type)))))
           exp
           (deref-check-err pos "expression is not assignable"))]
      [(stx:addr-exp var pos)
       (if (stx:var-exp? var)
           exp
           (deref-check-err pos "cannot take the address"))]
      [else exp]))
  (define (deref-check-err pos msg)
    (error '|dereference check error| (err-msg pos msg)))
  (traverse check-decl check-stmt check-exp ast))
