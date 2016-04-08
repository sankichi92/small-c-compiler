#lang racket
(require parser-tools/lex
         (prefix-in stx: "syntax.rkt")
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
  (let ([cur-lev 0])
    (define initial-env (lambda (x) #f))
    (define (register env decl)
      (lambda (name)
        (if (eq? name (decl-name decl))
            decl
            (env name))))
    (define (resolve-decl-list env decl-list)
      (if (null? decl-list)
          (cons '() env)
          (let* ([decl (car decl-list)]
                 [ret (resolve-decl env decl)]
                 [new-decl (car ret)]
                 [new-env (cdr ret)]
                 [rest-ret (resolve-decl-list new-env (cdr decl-list))]
                 [last-env (cdr rest-ret)])
            (cons (cons new-decl (car rest-ret)) last-env))))
    (define (resolve-decl env decl)
      (cond [(stx:var-decls? decl)
             (let ([ty (stx:var-decls-ty decl)]
                   [decl-list (stx:var-decls-decls decl)])
               (resolve-var-decl-list env decl-list ty))]
            [(stx:fun-decl? decl)
             (let* ([name (stx:fun-decl-name decl)]
                    [ret-ty (stx:fun-decl-ret-ty decl)]
                    [parm-tys (stx:fun-decl-parm-tys decl)]
                    [pos (stx:fun-decl-pos decl)]
                    [ret (env name)])
               (if ret
                   (let ([kind (decl-kind ret)]
                         [lev (decl-lev ret)]
                         [type (decl-type ret)])
                     (cond [(and (eq? 'var kind) (= 0 lev))
                            (redef-err pos name)]
                           [(and
                             (or (eq? 'proto kind) (eq? 'fun kind))
                             (not (equal? parm-tys (cddr type))))
                            (redef-err pos name)]
                           [(or (eq? 'proto kind) (eq? 'fun kind))
                            (cons '() env)]
                           [else
                            (register-proto env name ret-ty parm-tys pos)]))
                   (register-proto env name ret-ty parm-tys pos)))]
            [(stx:fun-def? decl)
             (let* ([name (stx:fun-def-name decl)]
                    [ret-ty (stx:fun-def-ret-ty decl)]
                    [parms (stx:fun-def-parms decl)]
                    ;[new-parms (resolve-param-list params)] ;TODO
                    [body (stx:fun-def-body decl)]
                    ;[new-body (resolve-cmpd-stmt body)] ;TODO
                    [pos (stx:fun-def-pos decl)]
                    [ret (env name)])
               (if ret
                   (let ([kind (decl-kind ret)]
                         [lev (decl-lev)]
                         [type (decl-type ret)])
                     (cond [(and (eq? 'var kind) (= 0 lev))
                            (redef-err pos name)]
                           [(and
                             (or (eq? 'proto kind) (eq? 'fun kind))
                             (not (equal? (map stx:parm-decl-ty parms) (cddr type))))
                            (redef-err pos name)]
                           [(eq? 'fun kind)
                            (cons '() env)]
                           [else
                            (register-fun env name ret-ty parms body pos)]))
                   (register-fun env name ret-ty parms body pos)))]
            )) ;TODO
    (define (resolve-var-decl-list env decl-list ty)
      (if (null? decl-list)
          (cons '() env)
          (let* ([decl (car decl-list)]
                 [ret (resolve-var-decl env decl ty)]
                 [new-decl (car ret)]
                 [new-env (cdr ret)]
                 [rest-ret (resolve-var-decl-list new-env (cdr decl-list) ty)]
                 [last-env (cdr rest-ret)])
            (cons (cons new-decl (car rest-ret)) last-env))))
    (define (resolve-var-decl env decl ty1)
      (let* ([name (stx:var-decl-name decl)]
             [ty2 (stx:var-decl-ty decl)]
             [ty (format-type ty1 ty2)]
             [pos (stx:var-decl-pos decl)]
             [ret (env name)])
        (if ret
            (let ([kind (decl-kind ret)]
                  [lev (decl-lev ret)])
              (cond [(and (or (eq? kind 'fun) (eq? kind 'proto)) (= lev 0))
                     (redef-err pos name)]
                    [(and (eq? kind 'var) (= lev cur-lev))
                     (redef-err pos name)]
                    [else
                     (begin
                       (cond [(eq? kind 'parm)
                              (redef-warn pos name)])
                       (register-var env name ty pos))]))
            (register-var env name ty pos))))
    (define (register-var env name ty pos)
      (let* ([decl (decl name cur-lev 'var ty)]
             [new-env (register env decl)])
        (cons (stx:var-decl decl ty pos) new-env)))
    (define (register-proto env name ret-ty parm-tys pos)
      (let* ([decl (decl name 0 'proto (list* 'fun ret-ty parm-tys))]
             [new-env (register env decl)])
        (cons (stx:fun-decl decl ret-ty parm-tys pos) new-env)))
    (define (register-fun env name ret-ty parms body pos)
      (let* ([parm-tys (map stx:parm-decl-ty parms)]
             [type (list* 'fun ret-ty parm-tys)]
             [decl (decl name 0 'fun type)]
             [new-env (register env decl)])
        (cons (stx:fun-def decl ret-ty parms body pos) new-env)))
    (define (format-type ty1 ty2)
      (cond [(null? ty2)
             ty1]
            [(eq? 'array (car ty2))
             (list 'array ty1 (cdr ty2))]
            [(eq? 'pointer (car ty2))
             (list 'pointer (format-type ty1 (cdr ty2)))]))
    (define (redef-warn pos name)
      (eprintf (err-msg pos (format "name resolve warning: overwriting of parm ~a" name))))
    (define (redef-err pos name)
      (error (err-msg pos (format "name resolve error: redifinition of ~a" name))))
    (define (err-msg pos msg)
      (format "~a:~a: ~a\n" (position-line pos) (position-col pos) msg))
    (car (resolve-decl-list initial-env ast))))
