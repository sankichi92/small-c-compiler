#lang racket
(provide (all-defined-out))

(struct decl (name lev kind type) #:transparent)
