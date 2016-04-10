#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
         "utils.rkt"
         "traverser.rkt"
         (prefix-in nr: "name-resolver.rkt"))
(provide form-check form-check-str)

(define (form-check ast)
  (define (check-decl decl) decl)
  (define (check-stmt stmt) stmt)
  (define (check-exp exp)
    (match exp
      [(stx:assign-exp left right pos)
       (if (or (stx:deref-exp? left)
                (and (stx:var-exp? left)
                     (let* ([decl (stx:var-exp-tgt left)]
                            [type (nr:decl-type decl)]
                            [array? (if (eq? type 'int)
                                      #f
                                      (eq? (car type) 'array))])
                        (not array?))))
           exp
           (error
             'form-error
             (err-msg pos "operator '=' has only a variable, a pointer or an array as left operand")))]
      [(stx:addr-exp var pos)
       (if (stx:var-exp? var)
           exp
           (error
             'form-error
             (err-msg pos "operator '&' has only a variable, a pointer or an array as operand")))]
      [else exp]))
  (traverse check-decl check-stmt check-exp ast))

(define (form-check-str str)
  (form-check (nr:name-resolve-str str)))
