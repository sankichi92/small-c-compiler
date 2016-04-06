#lang racket
;; (require rackunit)
(provide (all-defined-out))

(struct program (program ex-decl pos) #:transparent)
(struct decl (type lis pos) #:transparent)
(struct pt-decl (decl pos) #:transparent)
(struct dir-decl (name pos) #:transparent)
(struct arr-decl (name num pos) #:transparent)
(struct prot-decl (ty name pos) #:transparent)
(struct func-decl (name params pos) #:transparent)
(struct pt-func-decl (name params pos) #:transparent)
(struct func-def (ty decl stmt pos) #:transparent)
(struct parm-decl (ty decl pos) #:transparent)
(struct parm-name (name pos) #:transparent)
(struct pt-parm-name (name pos) #:transparent)
(struct int-ty (pos) #:transparent)
(struct void-ty (pos) #:transparent)
(struct for-stmt (init test inc body pos) #:transparent)
(struct ret-stmt (val pos) #:transparent)
(struct assign-exp (left right pos) #:transparent)
(struct lop-exp (op left right pos) #:transparent)
(struct arr-exp (name index pos) #:transparent)
(struct func-exp (name args pos) #:transparent)

;; declaration/definition
;(struct parm-decl (name ty pos)                #:transparent)
;(struct fun-decl  (name ret-ty parm-tys pos)   #:transparent)
;(struct fun-def   (name ret-ty parms body pos) #:transparent)

;; statement

;; メモリへの書き込み: *<exp> = <exp>;
(struct massign-stmt (dst src pos)              #:transparent)
;; 変数への代入: <var> = <exp>;
(struct assign-stmt  (var src pos)              #:transparent)
;; 条件分岐: if(<exp>) <cmpd-stmt> else <cmpd-stmt>
(struct if-else-stmt      (test tbody ebody pos)     #:transparent)
(struct if-stmt      (test tbody pos)     #:transparent)
;; 繰り返し: while(<exp>) <cmpd-stmt>
(struct while-stmt   (test body pos)            #:transparent)
;; 値の出力: print(<exp>);
(struct print-stmt   (exp pos)                  #:transparent)

;; 複文: {<stmt>;...<stmt>;}
(struct cmpd-stmt    (decls stmts pos)                #:transparent)

;; expression
;; 変数: <var>
(struct var-exp    (tgt pos)           #:transparent)
;; 偽を表す真偽値: true
(struct true-exp   (pos)               #:transparent)
;; 偽を表す真偽値: false
(struct false-exp  (pos)               #:transparent)
;; 整数即値: <num>
(struct lit-exp    (val pos)           #:transparent)
;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp    (op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp    (op left right pos) #:transparent)
;; 符号反転: -<exp>
(struct neg-exp    (arg pos)           #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp  (var pos)           #:transparent)
(struct call-exp   (tgt args pos)      #:transparent)
