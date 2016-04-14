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
        (string-append "label" (number->string oldid))))))
