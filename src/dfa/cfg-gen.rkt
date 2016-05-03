#lang racket
(require (prefix-in ett: "../entity.rkt")
         (prefix-in ir: "../ir.rkt")
         (prefix-in cfg: "cfg.rkt"))
(provide ir->cfg)

;; ir中の各ラベルを直後の文へくっつける．
;; くっつけて出来るコンスペア (ラベル集合 . stmt) を以降「ラベル付き文」と呼ぶ．
;;   stmtリスト -> ラベル付き文のリスト
(define (coalesce-label ir)
  (define (ir->stmts ir)
    (define (decl->stmts decl)
      (match decl
        [(ir:fun-def var parms body)
         (let* ([stmts (ir:cmpd-stmt-stmts body)]
                [new-stmts (append-map format-stmt stmts)])
            `(,(ir:label-stmt (ett:decl-name var))
              ,@new-stmts))]
        [else '()]))
    (define (format-stmt stmt)
      (match stmt
        [(ir:cmpd-stmt _ stmts)
         (let ([new-stmts (append-map format-stmt stmts)])
           new-stmts)]
        [(ir:label-stmt name)
         (list (ir:label-stmt (string->symbol name)))]
        [(ir:if-stmt var tlabel elabel)
         (let ([sym-tl (string->symbol tlabel)]
               [sym-el (string->symbol elabel)])
           (list (ir:if-stmt var sym-tl sym-el)))]
        [(ir:goto-stmt label)
         (list (ir:goto-stmt (string->symbol label)))]
        [else (list stmt)]))
    (append-map decl->stmts ir))
  (define (coalesce-stmts stmts)
    ((compose1 reverse (lambda (l-stmts)
                       (map (lambda (l-stmt)
                              (cons (reverse (car l-stmt))
                                    (cdr l-stmt)))
                            l-stmts)))
     (foldl (lambda (stmt l-stmts)
              (if (null? l-stmts)
                  (if (ir:label-stmt? stmt)
                      (cons (cons (list (ir:label-stmt-name stmt))
                                  #f)
                            l-stmts)
                      (cons (cons '() stmt) l-stmts))
                  (let ([i (first l-stmts)]
                        [is (rest l-stmts)])
                    (cond
                      [(and (ir:label-stmt? stmt)
                            (not (cdr i)))
                       (cons (cons (cons (ir:label-stmt-name stmt) (car i))
                                   (cdr i))
                             is)]
                      [(ir:label-stmt? stmt)
                       (cons (cons (list (ir:label-stmt-name stmt))
                                   #f)
                             l-stmts)]
                      [(not (cdr i))
                       (cons (cons (car i) stmt)
                             is)]
                      [else (cons (cons '() stmt) l-stmts)]))))
            '()
            stmts)))
  (coalesce-stmts `(BEGIN ,@(ir->stmts ir) END)))

;; 基本ブロックの先頭にあたるラベル付き文をleaderと呼ぶ．
;; leader集合を見つけて返す
;;   ラベル付き文のリスト -> leader集合
(define (find-leaders l-ir)
  (define (find-target lbl)
    (first (memf (lambda (l-stmt)
                   (set-member? (car l-stmt) lbl))
                 l-ir)))
  (if (null? l-ir)
      '()
      ((compose1 reverse cdr)
       (foldl
        (lambda (l-stmt acc)
          (let* ([stmt (cdr l-stmt)]
                 [is-leader (car acc)]
                 [leaders (cdr acc)]
                 [leaders2 (if is-leader
                               (set-add leaders l-stmt)
                               leaders)])
            (match stmt
              ['BEGIN
               (cons #t
                     (set-add
                      leaders2
                      (find-target 'main)))]
              ['END (cons #f (set-add leaders2 l-stmt))]
              [(ir:if-stmt _ tlabel elabel)
               (cons #t
                     (set-union
                      leaders2
                      `(,(find-target tlabel)
                        ,(find-target elabel))))]
              [(ir:goto-stmt label)
               (cons #t
                     (set-add
                      leaders2
                      (find-target label)))]
              [(ir:ret-stmt _)
               (cons #t leaders2)]
              [(ir:call-stmt _ tgt _)
               (cons #t
                     (set-add
                      leaders2
                      (find-target (ett:decl-name tgt))))]
              [else
               (cons #f leaders2)])))
        (cons #t '())
        l-ir))))

; 基本ブロックのコンストラクタ
;;   ラベル付き文のリスト -> エッジ情報が未設定のbblock
(define (make-bblock l-stmts)
  (cfg:bblock (car (first l-stmts))
          (list->vector (map cdr l-stmts))
          '()
          '()))

;; leader情報をつかってラベル付き文のリストを基本ブロックに分割
;;   ラベル付き文のリスト，leader集合 -> bblockのリスト
(define (split l-ir leaders)
  (reverse
    (map (compose1 make-bblock reverse)
         (foldl (lambda (l-stmt bbs)
                  (if (set-member? leaders l-stmt)
                      (cons (list l-stmt) bbs)
                      (cons (cons l-stmt (first bbs))
                            (rest bbs))))
                '()
                l-ir))))

;; 制御フローに従い，基本ブロック間にエッジを張る
;;   bblockのリスト, u-lbls -> エッジの張られたbblockのベクタ(つまりCFG)
;;     u-lbls: 実際に使用されたラベルを記録するためのmutable set
(define (set-edges bbs u-lbls)
  (define (add-pred bb p) ;; bbのpredsにpを追加
    (cfg:set-bblock-preds! bb (set-add (cfg:bblock-preds bb) p)))
  (define (add-succ bb s) ;; bbのsuccsにsを追加
    (cfg:set-bblock-succs! bb (set-add (cfg:bblock-succs bb) s)))
  (let* ([idx (range 0 (length bbs))]
         [end-idx (- (length bbs) 1)]
         [labels->idx (map cons
                           (map cfg:bblock-labels bbs)
                           idx)]
         [bbv (list->vector bbs)])
    ;; bbvのi,j番目の基本ブロック間に双方向のエッジを張る
    (define (add-edge i j)
      (add-succ (vector-ref bbv i) j)
      (add-pred (vector-ref bbv j) i))
    ;; 先頭ラベルにlblを含む基本ブロックを探し，そのインデックスを返す
    ;; また，見つかれば使用ラベル集合u-lblsにそのラベルを追加
    (define (find-target-idx lbl)
      (let ([idx (cdr (assf (lambda (lbls) (set-member? lbls lbl))
                            labels->idx))])
        (unless idx (error "target label not found:" lbl))
        (set-add! u-lbls lbl)
        idx))
    (set-for-each
      u-lbls
      (lambda (l)
        (add-edge 0 (find-target-idx l))))
    (for-each ;; 基本ブロックリスト(とそのインデックス)を先頭から順に処理
      (lambda (bb i)
        (let* ([stmts (cfg:bblock-stmts bb)]
               [last-stmt (vector-ref stmts (- (vector-length stmts) 1))])
          (match last-stmt ;; 基本ブロックの末尾の文で場合分けしながら，適切なエッジを張る
            ['BEGIN
             (let ([mi (find-target-idx 'main)])
               (add-edge i mi))]
            ['END (void)]
            [(ir:if-stmt _ tlabel elabel)
             (let ([ti (find-target-idx tlabel)]
                    [ei (find-target-idx elabel)])
               (add-edge i ti)
               (add-edge i ei))]
            [(ir:goto-stmt label)
             (let ([ti (find-target-idx label)])
               (add-edge i ti))]
            [(ir:ret-stmt _) (add-edge i end-idx)]
            [(ir:call-stmt _ tgt _)
             (let ([fi (find-target-idx (ett:decl-name tgt))])
               (add-edge 0 fi)
               (add-edge i (+ i 1)))]
            [else (add-edge i (+ i 1))])))
      bbs idx)
    bbv))

;; 各基本ブロックの先頭ラベル集合から未使用ラベルを取り除く
(define (gc-label cfg u-lbls)
  (for/vector ((bb cfg))
    (cfg:set-bblock-labels!
      bb
      (filter (lambda (lbl) (set-member? u-lbls lbl))
              (cfg:bblock-labels bb))))
  cfg)

;; 中間表現(stmtリスト) -> CFG
(define (ir->cfg ir)
  (let* ([used-labels (mutable-seteq)]
         [l-ir (coalesce-label ir)]
         [leaders (find-leaders l-ir)]
         [bbs (split l-ir leaders)]
         [e-bbs (set-edges bbs used-labels)])
    (gc-label e-bbs used-labels)))
