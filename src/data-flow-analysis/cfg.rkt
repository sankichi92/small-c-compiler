;;;; 制御フローグラフ(control flow graph)
#lang racket
(require (prefix-in ett: "../entity.rkt")
         (prefix-in ir: "../ir.rkt"))
(provide (all-defined-out))

;; 基本ブロック (basic block)
(struct bblock
  ((labels #:mutable) ; 先頭ラベル集合
   stmts              ; ブロック内stmtのベクタ
   (preds #:mutable)  ; predecessors (CFG中のインデックスの集合)
   (succs #:mutable)) ; successors (CFG中のインデックスの集合)
  #:transparent)
