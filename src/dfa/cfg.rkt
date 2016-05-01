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

;; CFGに含まれる全ての文をリストにして返す
(define (all-stmts cfg)
  (append-map (lambda (bb)
                (vector->list (bblock-stmts bb)))
              (vector->list cfg)))

;; CFG中のtgt-stmtの位置(インデックスの組)を返す
;;   CFG, stmt -> (bblockインデックス . stmtインデックス)
(define (find-stmt cfg tgt-stmt)
  ;; 基本ブロックbb中のtgt-stmtの位置(stmtインデックス)のリストを返す
  (define (find-in-bb bb)
    (let* ([stmts (bblock-stmts bb)]
           [jss (map (lambda (j stmt)
                       (cons j stmt))
                     (range 0 (vector-length stmts))
                     (vector->list stmts))])
      (map (lambda (js) (car js))
           (filter (lambda (js)
                     (let ((j (car js)) (stmt (cdr js)))
                       (eq? stmt tgt-stmt)))
                   jss))))
  (let ([rs (append-map (lambda (i bb)
                          (map (lambda (j) (cons i j))
                               (find-in-bb bb)))
                        (range 0 (vector-length cfg))
                        (vector->list cfg))])
    (unless (= (length rs) 1)
      (error "multiple or no stmts found:" rs))
    (first rs)))

;; stmtのpredecessorsを返す
;;   CFG, stmt -> stmtのリスト
(define (preds cfg stmt)
  (let* ([idx (find-stmt cfg stmt)]
         [b-idx (car idx)]
         [bb (vector-ref cfg b-idx)]
         [stmts (bblock-stmts bb)]
         [s-idx (cdr idx)])
    (cond
      [(and (zero? b-idx) (zero? s-idx)) ;; BEGIN
       '()]
      [(zero? s-idx) ;; 基本ブロックの先頭の文
       (map (lambda (pb-idx)
              (let* ((pb (vector-ref cfg pb-idx))
                     (stmts (bblock-stmts pb)))
                (vector-ref stmts (- (vector-length stmts) 1))))
            (bblock-preds bb))]
      [else ;; 基本ブロックの先頭以外の文
       (list (vector-ref stmts (- s-idx 1)))])))

;; stmtのsuccessorsを返す
;;   CFG, stmt -> stmtのリスト
(define (succs cfg stmt)
  (let* ([idx (find-stmt cfg stmt)]
         [b-idx (car idx)]
         [bb (vector-ref cfg b-idx)]
         [stmts (bblock-stmts bb)]
         [s-idx (cdr idx)])
    (cond
      [(and (= b-idx (- (vector-length cfg) 1))
            (= s-idx (- (vector-length stmts) 1))) ;; END
       '()]
      [(= s-idx (- (vector-length stmts) 1)) ;; 基本ブロックの末尾の文
       (map (lambda (sb-idx)
              (let* ((sb (vector-ref cfg sb-idx))
                     (stmts (bblock-stmts sb)))
                (vector-ref stmts 0)))
            (bblock-succs bb))]
      [else ;; 基本ブロックの末尾以外の文
       (list (vector-ref stmts (+ s-idx 1)))])))
