;;;; 到達可能定義解析 (reachable definition analysis)
#lang racket
(require (prefix-in ett: "../entity.rkt")
         (prefix-in ir: "../ir.rkt")
         (prefix-in dfa: "dfa.rkt"))
