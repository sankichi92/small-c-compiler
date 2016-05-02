;;;; 定数畳み込み (constant folding)
#lang racket
(require (prefix-in ett: "../entity.rkt")
         (prefix-in ir:  "../ir.rkt")
         (prefix-in dfa:  "../dfa/dfa.rkt")
         "../dfa/reachable-def-anl.rkt")
(provide constant-fold)

(define (constant-fold ir)
  (let ([solution (dfa:solve (analysis ir) ir)])
    (define (fold-decl decl)
      (match decl
        [(ir:var-decl _) decl]
        [(ir:fun-def var parms body)
         (let ([folded-body (fold-stmt body)])
           (ir:fun-def var parms folded-body))]))
    (define (fold-stmt stmt)
      (match stmt
        [(ir:cmpd-stmt decls stmts)
         (let ([folded-stmts (map fold-stmt stmts)])
           (ir:cmpd-stmt decls folded-stmts))]
        [(ir:assign-stmt var exp)
         (let* ([prop (dfa:get-property solution stmt #:kind 'before)]
                [folded-exp (fold-exp prop exp)])
           (ir:assign-stmt var folded-exp))]
        [else stmt]))
    (define (fold-exp prop exp)
      (match exp
        [(ir:var-exp var)
         (let ([folded-var (fold-var prop var)])
           (if folded-var
               (ir:lit-exp folded-var)
               exp))]
        [(ir:aop-exp op left right)
         (let ([l (fold-var prop left)]
               [r (fold-var prop right)])
           (if (and l r)
               (let ([val (match op
                            ['+ (+ l r)]
                            ['- (- l r)]
                            ['* (* l r)]
                            ['/ (/ l r)])])
                  (ir:lit-exp val))
               exp))]
        [(ir:rop-exp op left right)
         (let ([l (fold-var prop left)]
               [r (fold-var prop right)])
           (if (and l r)
               (let ([val (match op
                            ['== (if (= l r) 1 0)]
                            ['!= (if (= l r) 0 1)]
                            ['<  (if (< l r) 1 0)]
                            ['<= (if (<= l r) 1 0)]
                            ['>  (if (> l r) 1 0)]
                            ['>= (if (>= l r) 1 0)])])
                 (ir:lit-exp val))
               exp))]
        [else exp]))
    (define (fold-var prop var)
      (let* ([dummy-defs (dict-ref prop var '())]
             [defs (set-remove dummy-defs 'dummy)])
        (if (= (set-count defs) 1)
            (let ([def (set-first defs)])
              (match def
                [(ir:assign-stmt _ _)
                 (let* ([folded-astmt (fold-stmt def)]
                        [folded-exp (ir:assign-stmt-exp folded-astmt)])
                   (if (ir:lit-exp? folded-exp)
                       (ir:lit-exp-val folded-exp)
                       #f))]
                [(ir:write-stmt _ src)
                 (if (eq? (ett:decl-kind var) 'parm)
                     #f
                     (let ([prop (dfa:get-property solution def #:kind 'before)])
                        (fold-var prop src)))]
                ['any #f]))
            #f)))
    (map fold-decl ir)))
