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
  )
