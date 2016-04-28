#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "parser.rkt"
         "name-resolver.rkt"
         "deference-checker.rkt"
         "type-checker.rkt"
         "ir-generator.rkt"
         "addr-assigner.rkt"
         "code-generator.rkt")
(provide compile test-file test-string)

(define (comp port #:phase [phase 'asm])
  (define phases
    `((parse      . ,parse-port)
      (resolve    . ,name-resolve)
      (def-check  . ,deference-check)
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
    (aux port phases)))

(define (test-string str #:phase [phase 'asm])
  (let ([port (open-input-string str)])
  (comp port #:phase phase)))

(define (test-file fname #:phase [phase 'asm])
  (let* ([port (open-input-file fname)])
    (comp port #:phase phase)))

(define (compile fname)
  (let ([str (test-file fname)])
    (display str)
    (newline)))
