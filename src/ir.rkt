#lang racket
(provide (all-defined-out))

;; declaration/definition
(struct var-decl (var) #:transparent)
(struct fun-def (var parms body) #:transparent)

;; statement
(struct assign-stmt (var exp) #:transparent)
(struct write-stmt (dest src) #:transparent)
(struct read-stmt (dest src) #:transparent)
(struct label-stmt (name) #:transparent)
(struct if-stmt (var tlabel elabel) #:transparent)
(struct goto-stmt (label) #:transparent)
(struct call-stmt (dest tgt vars) #:transparent)
(struct ret-stmt (var) #:transparent)
(struct print-stmt (var) #:transparent)
(struct cmpd-stmt (decls stmts) #:transparent)

;; expression
(struct var-exp (var) #:transparent)
(struct lit-exp (val) #:transparent)
(struct aop-exp (op left right) #:transparent)
(struct rop-exp (op left right) #:transparent)
(struct addr-exp (var) #:transparent)
