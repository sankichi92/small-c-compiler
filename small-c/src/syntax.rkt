#lang racket
(provide (all-defined-out))

;; declaration/definition

(struct var-decls (ty decls pos)               #:transparent)
(struct var-decl  (name ty pos)                #:transparent)
(struct parm-decl (name ty pos)                #:transparent)
(struct fun-decl  (name ret-ty parm-tys pos)   #:transparent)
(struct fun-def   (name ret-ty parms body pos) #:transparent)

;; statement

;(struct if-stmt     (test tbody pos)          #:transparent)
(struct if-els-stmt (test tbody ebody pos)    #:transparent)
(struct while-stmt  (test body pos)           #:transparent)
;(struct for-stmt    (init test inc body pos)  #:transparent)
(struct ret-stmt    (exp pos)                 #:transparent)
(struct cmpd-stmt   (decls stmts pos)         #:transparent)
;(struct print-stmt (exp pos) #:transparent)

;; expression

(struct assign-exp  (left right pos)    #:transparent)
(struct lop-exp     (op left right pos) #:transparent)
(struct rop-exp     (op left right pos) #:transparent)
(struct aop-exp     (op left right pos) #:transparent)
;(struct neg-exp     (arg pos)           #:transparent)
(struct addr-exp    (var pos)           #:transparent)
(struct deref-exp   (arg pos)           #:transparent)
;(struct arr-exp     (name idx pos)      #:transparent)
(struct fun-exp     (name args pos)     #:transparent)
(struct var-exp     (tgt pos)           #:transparent)
(struct lit-exp     (val pos)           #:transparent)
