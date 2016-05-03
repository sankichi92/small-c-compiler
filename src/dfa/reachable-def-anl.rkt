;;;; 到達可能定義解析 (reachable definition analysis)
#lang racket
(require (prefix-in ett: "../entity.rkt")
         (prefix-in ir: "../ir.rkt")
         (prefix-in dfa: "dfa.rkt"))
(provide analysis)

;; -> store
(define (empty-store) (hasheq))

(define bot-store (empty-store))

;; store -> 変数名のリスト
(define store-vars dict-keys)

;; store, 変数名 -> 到達可能定義集合
(define (store-lookup s v)
  (dict-ref s v '()))

;; store, 変数名，到達可能定義集合 -> store
(define store-update dict-set)

;; store, 変数名 -> store
(define store-remove dict-remove)

(define (store-add-all-int store def)
  (define (store-update-int store i)
    (if i
        (let ([var (dict-iterate-key store i)])
          (if (eq? (ett:decl-type var) 'int)
              (let* ([def-set (store-lookup store var)]
                     [new-set (set-add def-set def)]
                     [new-store (store-update store var new-set)])
                (store-update-int new-store (dict-iterate-next store i)))
              (store-update-int store (dict-iterate-next store i))))
        store))
  (store-update-int store (dict-iterate-first store)))

;; dfa.rktで指定されている仕様に沿った比較結果(-1/0/1/#f)を返す
(define (store-compare left right)
  (define (leq l r)
    (andmap (lambda (v)
              (subset? (store-lookup l v)
                       (store-lookup r v)))
            (store-vars l)))
  (let ([leq-l (leq left right)]
        [leq-r (leq right left)])
    (cond [(and leq-l leq-r) 0]
          [leq-l -1]
          [leq-r 1]
          [else #f])))

;; storeのleast upper bound演算
(define (store-lub . ss)
  (foldl (lambda (ps ns)
           (foldl (lambda (v ns)
                    (let ([p-set (store-lookup ps v)]
                          [n-set (store-lookup ns v)])
                      (store-update ns v (set-union p-set n-set))))
                  ns
                  (store-vars ps)))
         bot-store
         ss))

;; 伝達関数，forward store update
;; stmt，store -> store
(define (transfer stmt entry-store)
  (define (gen s)
    (match stmt
      [(ir:assign-stmt var _)
       (store-update s var (list stmt))]
      [(ir:write-stmt dest src)
       (store-add-all-int s stmt)]
      [(ir:read-stmt dest _)
       (store-update s dest '(any))]
      [(ir:call-stmt dest tgt _)
       (let ([args (cddr (ett:decl-type tgt))])
         (if (ormap (lambda (a)
                      (and (list? a) (eq? (first a) 'pointer)))
                    args)
             (store-update (store-add-all-int s 'any) dest '(any))
             (store-update s dest '(any))))]
      [else s]))
  (define (kill s)
    (match stmt
      [(ir:assign-stmt var _)
       (store-remove s var)]
      [(ir:write-stmt dest _)
       (store-remove s dest)]
      [(ir:read-stmt dest _)
       (store-remove s dest)]
      [(ir:call-stmt dest _ _)
       (store-remove s dest)]
      [else s]))
  (gen (kill entry-store)))

;; ir -> 出現する全ての変数名のリスト
(define (ir->vs ir)
  (define (decl->vs decl)
    (match decl
      [(ir:fun-def _ parms body)
       (stmt->vs body)]
      [else '()]))
  (define (stmt->vs stmt)
    (match stmt
      [(ir:cmpd-stmt decls stmts)
       (set-union (map ir:var-decl-var decls)
                  (append-map stmt->vs stmts))]
      [else '()]))
  (apply set-union (map decl->vs ir)))

;; ir -> analysis
(define (analysis ir)
  (let* ([vs (ir->vs ir)]
         [init (foldl (lambda (v s) ; vs -> store
                        (store-update s v '(dummy)))
                      bot-store
                      vs)])
    (dfa:analysis 'forward
                  transfer
                  store-compare
                  store-lub
                  bot-store
                  init)))
