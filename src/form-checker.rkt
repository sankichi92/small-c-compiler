lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         "utils.rkt"
         "traverser.rkt"
         "name-resolver.rkt")
(provide form-check)

(define (form-check ast)
  (define (check-decl decl) decl)
  (define (check-stmt stmt) stmt)
  (define (check-exp exp)
    (cond [(stx:assign-exp? exp) exp]
          [(stx:addr-exp? exp) exp]
          [else exp]))
  (traverse check-decl check-stmt check-exp ast))
