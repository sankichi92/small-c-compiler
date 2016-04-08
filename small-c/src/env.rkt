#lang racket
(require (prefix-in stx: "syntax.rkt")
         (prefix-in parse: "parser.rkt"))
(provide (all-defined-out))

; name: オブジェクトの名前（シンボル）
; lev: ブロックの深さ
;   - 大域変数と（大域）関数のレベルは 0
;   - 関数のパラメータ宣言のレベルは 1
;   - 関数本体のブロックのレベルは 2
;   - 関数本体中の入れ子ブロックのレベルは3以上（深さに応じる）
; kind: オブジェクトの種類
;   - var: 変数
;   - parm: パラメータ
;   - fun: 関数定義
;   - proto: プロトタイプ宣言
; type: オブジェクトの型情報
;   - int: int型
;   - (pointer ‹t›): 参照先の型が‹t›であるようなポインタの型
;   - (array ‹t› ‹n›): 要素の型が‹t›，サイズが‹n›であるような配列型
;   - (fun ‹r› ‹a1› ... ‹an›): 返り値の型が‹r›，引数の型がそれぞれ‹a1› ... ‹an›
;       であるようなn引数関数の型．ただし，値を返さない関数の場合，‹r›はvoidとする．

(struct decl (name lev kind type) #:transparent)

(define (name-resolver ast)
  (define initial-env (lambda (x) #f))
  (define (register env decl)
    (lambda (x)
      (if (equal? x (decl-name decl))
          decl
          (env decl))))
  (define (resolve-dcl-list env dcl-list)
    (if (null? dcl-list)
        (cons '() env)
        (let* ([dcl (car dcl-list)]
               [ret (resolve-dcl env dcl)]
               [new-dcl (car ret) ]
               [new-env (cdr ret)]
               [rest-ret (resolve-dcl-list new-env (cdr dcl-list))]
               [last-env (cdr rest-ret)])
          (cons (cons new-dcl (car rest-ret)) last-env))))
  (define (resolve-dcl env dcl)
    (cond [(stx:dcl? dcl)
           (let* ([ty (stx:dcl-ty dcl)]
                  [dcrs (stx:dcl-dcrs dcl)]
                  [ret (resolve-dcr-list env dcrs)]
                  [new-dcl (car ret)]
                  [new-env (cdr ret)])
              (cons new-dcl new-env))]
          ;[(stx:proto? dcl)
          ; (let ([ty (stx:proto-ty dcl)]
          ;        [dcr (stx:proto-dcr dcl)]))] ;TODO
          ;[(stx:fun-def? dcl)
          ; (let ([ty (stx:fun-def-ty dcl)]
          ;        [dcr (stx:fun-def-dcr dcl)]
          ;        [stmts (stx:fun-def-stmts dcl)]))]
    )) ;TODO
  (define (resolve-dcr-list env dcr-list)
    (if (null? dcr-list)
        (cons '() env)
        (let* ([dcr (car dcr-list)]
               [ret (resolve-dcr env dcr 'int)]
               [new-dcr (car ret)]
               [new-env (cdr ret)]
               [rest-ret (resolve-dcr-list new-env (cdr dcr-list))]
               [last-env (cdr rest-ret)])
          (cons (cons new-dcr (car rest-ret)) last-env))))
  (define (resolve-dcr env dcr ty)
    (cond [(stx:pt-dcr? dcr)
           (let ([new-dcr (stx:pt-dcr-dcr dcr)])
              (resolve-dcr env new-dcr (list 'pointer ty)))]
          [(stx:arr-dcr? dcr)
           (let* ([name (stx:arr-dcr-name dcr)]
                  [num (stx:arr-dcr-num dcr)]
                  [ret (env name)])
              (if ret
                  (let ([kind (decl-kind ret)]
                        [lev (decl-lev ret)])
                    (cond [(and (or (eq? kind 'fun) (eq? kind 'proto)) (= lev 0))
                           (display "Error:Redifinition" (current-error-port))]
                          [(and (eq? kind 'var) (= lev 0))
                           (display "Error:Redifinition" (current-error-port))]
                          [else
                           (begin
                             (cond [(eq? kind 'parm)
                                    (display "Warning:" (current-error-port))])
                             (let* ([decl (decl name 0 'var (list 'array ty num))]
                                    [new-env (register env decl)])
                                (cons decl new-env)))]))
                  (let* ([decl (decl name 0 'var (list 'array ty num))]
                         [new-env (register env decl)])
                    (cons decl new-env))))]
          [(stx:dcr? dcr)
           (let* ([name (stx:dcr-name dcr)]
                  [ret (env name)])
              (if ret
                (let ([kind (decl-kind ret)]
                      [lev (decl-lev ret)])
                  (cond [(and (or (eq? kind 'fun) (eq? kind 'proto)) (= lev 0))
                         (display "Error:Redifinition" (current-error-port))]
                        [(and (eq? kind 'var) (= lev 0))
                         (display "Error:Redifinition" (current-error-port))]
                        [else
                         (begin
                           (cond [(eq? kind 'parm)
                                  (display "Warning:" (current-error-port))])
                           (let* ([decl (decl name 0 'var ty)]
                                  [new-env (register env decl)])
                              (cons decl new-env)))]))
                (let* ([decl (decl name 0 'var ty)]
                       [new-env (register env decl)])
                  (cons decl new-env))))]))
  (car (resolve-dcl-list initial-env ast)))