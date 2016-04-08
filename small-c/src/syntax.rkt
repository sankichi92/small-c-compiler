#lang racket
;; (require rackunit)
(provide (all-defined-out))

;; declaration/definition

(struct dcl       (ty dcrs pos)       #:transparent)
(struct proto     (ty dcr pos)        #:transparent)
(struct fun-def   (ty dcr stmts pos)  #:transparent)
(struct parm-dcl  (ty dcr pos)        #:transparent)

;; declarator

(struct dcr         (name pos)        #:transparent)
(struct pt-dcr      (dcr pos)         #:transparent)
(struct arr-dcr     (name num pos)    #:transparent)
(struct fun-dcr     (name parms pos)  #:transparent)
(struct fun-pt-dcr  (name parms pos)  #:transparent)
(struct parm-dcr    (name pos)        #:transparent)
(struct parm-pt-dcr (name pos)        #:transparent)

;; type

(struct int-ty  (pos) #:transparent)
(struct void-ty (pos) #:transparent)

;; statement

;(struct if-stmt     (test tbody pos)          #:transparent)
(struct if-els-stmt (test tbody ebody pos)    #:transparent)
(struct while-stmt  (test body pos)           #:transparent)
;(struct for-stmt    (init test inc body pos)  #:transparent)
(struct ret-stmt    (exp pos)                 #:transparent)
(struct cmpd-stmt   (dcls stmts pos)         #:transparent)
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
