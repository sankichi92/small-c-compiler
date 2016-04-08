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
