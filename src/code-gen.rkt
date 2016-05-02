#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt")
         "asm.rkt"
         "utils.rkt"
         "addr-assigner.rkt")
(provide ir->code code->string)

(define (ir->code addr-ir)
  (define offset 4)
  (define ($ r) `($ ,r))
  (define (-> p i) `(-> ,p ,i))
  (define (emit-label lbl)
    `(#:label ,lbl))
  (define (emit op . args)
    `(,op ,@args))
  (define (ofs-addr obj)
    (-> ($ fp) (ett:decl-offset obj)))
  (define ($sp num)
    (-> ($ sp) (* num offset)))
  (define ($a num)
    (let* ([num-str (number->string num)]
           [str (string-append "a" num-str)]
           [sym (string->symbol str)])
      ($ sym)))
  (define (emit-load-store tgt obj instr)
    (match obj
      [(ett:decl name _ 'var _ '())
       (list (emit la ($ t0) (symbol->string name))
             (emit instr ($ tgt) (-> ($ t0) 0)))]
      [(ett:decl _ lev 'parm _ '())
       (if (eq? instr 'lw)
           (list (emit move ($ tgt) ($a lev)))
           (list (emit move ($a lev) ($ tgt))))]
      [else
       (list (emit instr ($ tgt) (ofs-addr obj)))]))
  (define (emit-load dest obj)
    (emit-load-store dest obj lw))
  (define (emit-store src obj)
    (emit-load-store src obj sw))
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
           ,(emit sw ($ ra) ($sp 1))
           ,(emit sw ($ fp) ($sp 0))
           ,(emit addiu ($ fp) ($ sp) (- frame rest-parms-size offset))
           ,@(stmt->code body (if (> parms-length 4) 4 parms-length))
           ,(emit lw ($ fp) ($sp 0))
           ,(emit lw ($ ra) ($sp 1))
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
       (let* ([emit-load-store-arg (lambda (num instr)
                                     (emit instr ($a num) ($sp (+ num 2))))]
              [emit-load-arg (lambda (num)
                               (emit-load-store-arg num lw))]
              [emit-store-arg (lambda (num)
                                (emit-load-store-arg num sw))]
              [arg-list (build-list args-size values)])
         `(,@(map emit-store-arg arg-list)
           ,@(car (foldl (lambda (arg res)
                           (let ([i (cdr res)])
                             (cons
                               (append
                                 (car res)
                                 (if (>= i 4)
                                     `(,@(emit-load t0 arg)
                                       ,(emit sw ($ t0) ($sp (- i (length vars)))))
                                     (emit-load (second ($a i)) arg)))
                               (add1 i))))
                         (cons '() 0)
                         vars))
           ,(emit jal (ett:decl-name tgt))
           ,@(map emit-load-arg arg-list)
           ,@(if (eq? (second (ett:decl-type tgt)) 'void)
                 '()
                 (emit-store v0 dest))))]
      [(ir:ret-stmt var)
       (emit-load v0 var)]
      [(ir:print-stmt var)
       `(,@(emit-load a0 var)
         ,(emit li ($ v0) 1)
         ,(emit syscall)
         ,(emit la ($ a0) 'nl)
         ,(emit li ($ v0) 4)
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
    ,(emit-label "nl")
    ,(emit .asciiz "\"\n\"")
    ,(emit .text)
    ,(emit .globl "main")
    ,@(append-map fun-def->code addr-ir)))

(define (code->string code)
  (define (instr->string instr)
    (match (first instr)
      ['#:label (format "~a:" (second instr))]
      [else
       (format "\t~a\t~a"
               (symbol->string (first instr))
               (string-join (map arg->string (rest instr))
                            ", "))]))
  (define (arg->string arg)
    (cond [(string? arg) arg]
          [(symbol? arg) (symbol->string arg)]
          [(number? arg) (number->string arg)]
          [(eq? (first arg) '$)
           (format "$~a" (second arg))]
          [(eq? (first arg) '->)
           (format "~a(~a)" (third arg) (arg->string (second arg)))]
          [else (format "gen: unknown argument: ~a" arg)]))
  (string-join (map instr->string code) "\n"))
