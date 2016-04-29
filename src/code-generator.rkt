#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "asm.rkt"
         "utils.rkt"
         "addr-assigner.rkt")
(provide ir->code code->string)

(define (ir->code addr-ir)
  (define offset 4)
  (define (a num)
    (let* ([num-str (number->string num)]
           [str (string-append "a" num-str)]
           [sym (string->symbol str)])
      sym))
  (define ($ r) `($ ,r))
  (define (-> p i) `(-> ,p ,i))
  (define (emit-label lbl)
    `(#:label ,lbl))
  (define (emit op . args)
    `(,op ,@args))
  (define (ofs-addr obj)
    (-> ($ fp) (ett:decl-offset obj)))
  (define (emit-load dest obj)
    (if (null? (ett:decl-offset obj))
        (let ([name (ett:decl-name obj)]
              [lev (ett:decl-lev obj)]
              [kind (ett:decl-kind obj)])
          (match kind
            ['var
             (list (emit la ($ t0) (symbol->string name))
                   (emit lw ($ dest) (-> ($ t0) 0)))]
            ['parm
             (list (emit move ($ dest) ($ (a lev))))]))
        (list (emit lw ($ dest) (ofs-addr obj)))))
  (define (emit-store src obj)
    (if (null? (ett:decl-offset obj))
        (let ([name (ett:decl-name obj)]
              [lev (ett:decl-lev obj)]
              [kind (ett:decl-kind obj)])
          (match kind
            ['var
             (list (emit la ($ t0) (symbol->string name))
                   (emit sw ($ src) (-> ($ t0) 0)))]
            ['parm
             (list (emit move ($ (a lev)) ($ src)))]))
        (list (emit sw ($ src) (ofs-addr obj)))))
  (define (emit-sl-args num instr)
    (if (= num 0)
        '()
        (let* ([new-num (if (> num 4)
                            3
                            (sub1 num))]
               [ret (emit instr ($ (a new-num)) (-> ($ sp) (* (+ new-num 2) offset)))]
               [rest-ret (emit-sl-args new-num instr)])
          (cons ret rest-ret))))
  (define (emit-store-args num)
    (emit-sl-args num sw))
  (define (emit-load-args num)
    (emit-sl-args num lw))
  (define (emit-args args [num 0])
    (if (null? args)
        '()
        (let* ([arg (car args)]
               [ret (if (> num 4)
                        `(,@(emit-load ($ t0) arg)
                          ,(emit sw
                                 ($ t0)
                                 (-> ($ sp) ((* (- (length args)) offset)))))
                        (emit-load (a num) arg))]
               [rest-ret (emit-args (cdr args) (add1 num))])
          (append ret rest-ret))))
  (define (var-decl->code decl)
    (match decl
      [(ir:var-decl var)
       (let* ([name (ett:decl-name var)]
              [name-str (symbol->string name)]
              [type (ett:decl-type var)])
         (if (and (list? type) (eq? (first type) 'array))
             (list (emit-label name-str)
                   `(,.word ,@(build-list (third type) values)))
             (list (emit-label name-str)
                   (emit .word 0))))]
      [else '()]))
  (define (fun-def->code decl)
    (match decl
      [(cons (ir:fun-def var parms body) local-size)
       (let* ([parms-length (length parms)]
              [parms-size (* parms-length offset)]
              [rest-parms-size (if (> parms-length 4)
                                   (- parms-size (* offset 4))
                                   0)]
              [frame-size (+ parms-size local-size (* offset 2))]
              [frame (if (< frame-size 24)
                         24
                         frame-size)])
         `(,(emit-label (symbol->string (ett:decl-name var)))
           ,(emit subu ($ sp) ($ sp) frame)
           ,(emit sw ($ ra) (-> ($ sp) offset))
           ,(emit sw ($ fp) (-> ($ sp) 0))
           ,(emit addiu ($ fp) ($ sp) (- frame rest-parms-size offset))
           ,@(stmt->code body parms-length)
           ,(emit lw ($ fp) (-> ($ sp) 0))
           ,(emit lw ($ ra) (-> ($ sp) offset))
           ,(emit addiu ($ sp) ($ sp) frame)
           ,(emit jr ($ ra))))]
      [else '()]))
  (define (stmt->code stmt args-size)
    (match stmt
      [(ir:assign-stmt var exp)
       `(,@(exp->code t0 exp)
         ,@(emit-store t0 var))]
      [(ir:write-stmt dest src)
       `(,@(emit-load t0 dest)
         ,@(emit-load t1 src)
         ,(emit sw ($ t1) (-> ($ t0) 0)))]
      [(ir:read-stmt dest src)
       `(,@(emit-load t0 src)
         ,(emit lw ($ t0) (-> ($ t0) 0))
         ,@(emit-store t0 dest))]
      [(ir:label-stmt name)
       (list (emit-label name))]
      [(ir:if-stmt test tlabel elabel)
       `(,@(emit-load t0 test)
         ,(emit beqz ($ t0) elabel)
         ,(emit j tlabel))]
      [(ir:goto-stmt label)
       (list (emit j label))]
      [(ir:call-stmt dest tgt vars)
       `(,@(emit-store-args args-size)
         ,@(emit-args vars)
         ,(emit jal (ett:decl-name tgt))
         ,@(emit-load-args args-size)
         ,@(if (eq? (second (ett:decl-type tgt)) 'void)
               '()
               (emit-store v0 dest)))]
      [(ir:ret-stmt var)
       (emit-load v0 var)]
      [(ir:print-stmt var)
       `(,@(emit-load a0 var)
         ,(emit li ($ v0) 1)
         ,(emit syscall))]
      [(ir:cmpd-stmt decls stmts)
       (append-map (lambda (s)
                     (stmt->code s args-size))
                   stmts)]))
  (define (exp->code dest exp)
    (match exp
      [(ir:var-exp var)
       (emit-load dest var)]
      [(ir:lit-exp val)
       (list (emit li ($ dest) val))]
      [(ir:aop-exp op left right)
       (let ([instr (match op
                      ['+ add]
                      ['- sub]
                      ['* mul]
                      ['/ div])])
         `(,@(emit-load t0 left)
           ,@(emit-load t1 right)
           ,(emit instr ($ dest) ($ t0) ($ t1))))]
      [(ir:rop-exp op left right)
       (let ([instr (match op
                      ['== seq]
                      ['!= sne]
                      ['<  slt]
                      ['<= sle]
                      ['>  sgt]
                      ['>= sge])])
         `(,@(emit-load t0 left)
           ,@(emit-load t1 right)
           ,(emit instr ($ dest) ($ t0) ($ t1))))]
      [(ir:addr-exp var)
       (if (null? (ett:decl-offset var))
           (let ([name (ett:decl-name var)]
                 [lev (ett:decl-lev var)]
                 [kind (ett:decl-kind var)])
             (match kind
               ['var
                (list (emit la ($ dest) (symbol->string name)))]
               ['parm
                (list (emit addiu ($ dest) ($ fp) (* (add1 lev) offset)))]))
           (list (emit addiu ($ dest) ($ fp) (ett:decl-offset var))))]
      [else (list 'error)]))
  `(,(emit .data)
    ,@(append-map var-decl->code addr-ir)
    ,(emit .text)
    ,(emit .globl "main")
    ,@(append-map fun-def->code addr-ir)))

(define (code->string code)
  '())
