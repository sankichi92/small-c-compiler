#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "name-resolver.rkt"
         "deference-checker.rkt"
         "type-chekcer.rkt"
         "ir-generator.rkt"
         "addr-assigner.rkt"
         "code-generator.rkt")
(provide compile test-string)

(define (comp str #:phase [phase 'asm])
  (define phases
    `((parse      . ,parse-string)
      (resolve    . ,name-resolve)
      (def-check  . ,defference-check)
      (ty-check   . ,type-check)
      (ir         . ,ast->ir)
      (addr       . ,addr-assign)
      (gen        . ,ir->code)
      (asm        . ,code->string)))
  (unless (assoc phase phases)
    (error "comp: unknown phase." phase))
  (letrec ([aux (lambda (ast remaining-phases)
                  (let ([r ((cdr (first remaining-phases)) ast)])
                    (if (eq? phase (car (first remaining-phases)))
                        r
                        (aux r (rest remaining-phases)))))])
    (aux str phases)))

(define (test-string str #:phase [phase 'asm])
  (comp str #:phase phase))

(define (compile file)
  (define (test-file fname #:phase [phase 'asm])
    (let* ([in (open-input-file fname)]
           [str (string-join (port->lines in))])
      (close-input-port in)
      (comp str #:phase phase)))
  (define (dump-str str)
    (display str)
    (newline))
  (dump-str (test-file fname)))
