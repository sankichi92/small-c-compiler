#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt"))
(provide addr-assign)

(define (addr-assign ir)
  (define offset 4)
  (define (addr-decl decl)
    (match decl
      [(ir:fun-def var parms body)
       (let* ([new-parms (let ([len (length parms)])
                           (map ett:set-decl-lev!
                                (map ir:var-decl-var parms)
                                (build-list len values))
                           (when (> len 4)
                                 (addr-var-decl-list (cddddr parms) 0))
                           parms)]
              [ret (addr-stmt body 0)]
              [new-body (car ret)]
              [size (- (cdr ret))])
         (cons (ir:fun-def var new-parms new-body) size))]
      [else decl]))
  (define (addr-stmt stmt ofs)
    (match stmt
      [(ir:cmpd-stmt decls stmts)
       (let* ([ret (addr-var-decl-list decls ofs)]
              [new-decls (car ret)]
              [new-ofs (cdr ret)]
              [min-ofs new-ofs]
              [new-stmts (map (lambda (s)
                                (let* ([ret (addr-stmt s new-ofs)]
                                       [new-stmt (car ret)]
                                       [new-min-ofs (cdr ret)])
                                  (when (< new-min-ofs min-ofs)
                                        (set! min-ofs new-min-ofs))
                                  new-stmt))
                              stmts)])
         (cons (ir:cmpd-stmt new-decls new-stmts) min-ofs))]
      [else (cons stmt ofs)]))
  (define (addr-var-decl-list var-decls ofs)
    (if (null? var-decls)
        (cons '() ofs)
        (let* ([var-decl (car var-decls)]
               [ret (addr-var-decl var-decl ofs)]
               [new-var-decl (car ret)]
               [new-ofs (cdr ret)]
               [rest-ret (addr-var-decl-list (cdr var-decls) new-ofs)]
               [last-ofs (cdr rest-ret)])
          (cons (cons new-var-decl (car rest-ret)) last-ofs))))
  (define (addr-var-decl var-decl ofs)
    (let* ([obj (ir:var-decl-var var-decl)]
           [kind (ett:decl-kind obj)]
           [type (ett:decl-type obj)])
      (cond [(eq? kind 'parm)
             (let ([new-ofs (+ ofs offset)])
               (ett:set-decl-offset! obj new-ofs)
               (cons var-decl new-ofs))]
            [(and (list? type)
                  (eq? (first type) 'array))
             (let* ([num (third type)]
                    [new-ofs (- ofs (* num offset))])
               (ett:set-decl-offset! obj (+ new-ofs offset))
               (cons var-decl new-ofs))]
            [else
             (ett:set-decl-offset! obj ofs)
             (cons var-decl (- ofs offset))])))
  (map addr-decl ir))
