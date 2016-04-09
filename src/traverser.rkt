#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt"))
(provide (all-defined-out))

(define (traverser decl-proc stmt-proc exp-proc ast)
  (define (traverse-decl decl)
    (cond [(stx:var-decl? decl) decl]
          [(stx:fun-decl? decl) decl]
          [(stx:fun-def? decl)
           (let* ([name (stx:fun-def-name decl)]
                  [ret-ty (stx:fun-def-ret-ty decl)]
                  [parms (stx:fun-def-parms decl)]
                  [body (stx:fun-def-body decl)]
                  [pos (stx:fun-def-pos decl)]
                  [new-params (map decl-proc parms)]
                  [new-body (traverse-stmt body)])
              (stx:fun-def name ret-ty new-params new-body pos))]
          [(stx:parm-decl? decl) decl]))
  (define (traverse-stmt stmt) stmt)
  (map decl-proc (map traverse-decl ast)))
