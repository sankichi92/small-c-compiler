;;;; Generic Data Flow Analysis (a.k.a Monotone Framework)
#lang racket
(require data/queue
         (prefix-in cfg: "cfg.rkt"))
(provide analysis solve)

(struct analysis
  (direction     ;; 'forward or 'backward
   transfer      ;; 伝達関数: stmt, プロパティ -> プロパティ
   prop-compare  ;; プロパティの比較関数:
                 ;;   順序がつくなら，-1, 0, 1のいずれかを返す
                 ;;   順序がつかないなら，#fを返す
   lub           ;; プロパティのleast upper bound関数
   bot           ;; 最小プロパティ
   init          ;; 初期プロパティ
   )
  #:transparent)

;; データフロー解析を実行
;; analysis, CFG -> 解析結果
;;   解析結果: (辞書(stmt -> 直前のプロパティ) . 辞書(stmt -> 直後のプロパティ))
(define (solve anlys cfg)
  (let ([direction    (analysis-direction    anlys)]
        [transfer     (analysis-transfer     anlys)]
        [prop-compare (analysis-prop-compare anlys)]
        [lub          (analysis-lub          anlys)]
        [bot          (analysis-bot          anlys)]
        [init         (analysis-init         anlys)])
    ;; プロパティ辞書からstmtに対応するプロパティを取得．なければbotを返す
    (define (get-prop es stmt)
      (dict-ref es stmt bot))
    ;; ワークリストwlが空になるまでプロパティ辞書entries, exitsの更新を繰り返す．
    ;; directionがforwardの場合，entriesは直前のプロパティの辞書，exitsは直後の
    ;; プロパティの辞書．backwardの場合は逆．
    ;; ワークリストが空になった時点のentries, exitsが不動点，すなわち解析結果．
    ;; ワークリストは (stmt . プロパティ)のキュー．stmtの直前のプロパティへの
    ;; 追加を意味する．
    (define (fixed-point wl entries exits)
      (if (queue-empty? wl)
          (if (eq? direction 'forward)
              (cons entries exits)  ;; 直前=entries，直後=exits
              (cons exits entries)) ;; 直前=exits，直後=entries
          (let* ([w (dequeue! wl)]
                 [stmt (car w)]
                 [old-entry (get-prop entries stmt)]
                 [new-entry (lub old-entry (cdr w))]) ;; 追加を考慮し再計算
            (case (prop-compare new-entry old-entry) ;; 差分の有無で場合分け
              [(0) (fixed-point wl entries exits)]
              [(1) (let ([old-exit (get-prop exits stmt)]
                         [new-exit (transfer stmt new-entry)])
                     (case (prop-compare new-exit old-exit) ;; 差分の有無で場合分け
                       [(0) (fixed-point
                              wl
                              (dict-set entries stmt new-entry)
                              exits)]
                       [(1) (begin
                              (for-each (lambda (succ) ;; wlにsuccsを追加
                                          (enqueue! wl (cons succ new-exit)))
                                        (cfg:succs cfg stmt))
                              (fixed-point
                                wl
                                (dict-set entries stmt new-entry)
                                (dict-set exits stmt new-exit)))]
                       [else (error "transfer not monotone")]))]
              [else (error "lub operation not adequate")]))))
    (let* ([stmts ((compose1 (if (eq? direction 'forward)
                                 identity
                                 reverse)
                             cfg:all-stmts)
                   cfg)]
           ;; entries, exitsの初期値(すべての文のプロパティがbot)
           [init-prop (foldl (lambda (stmt prop)
                               (dict-set prop stmt bot))
                             (hasheq)
                             stmts)]
           [wl (make-queue)])
      (enqueue! wl (cons (first stmts) init))
      (fixed-point wl
                   init-prop
                   init-prop))))
