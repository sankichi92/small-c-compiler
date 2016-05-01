;;;; 定数畳み込み (constant folding)
#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         (prefix-in dfa:  "../dfa/dfa.rkt")
         "reachable-def-anl")
(provide constant-fold)

(define (constant-fold ir)
  )
