#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt"))
(provide addr-assign)

(define (addr-assign ir)
  (define offset 4)
  (define (addr-decl decl)
    (match decl
      [(ir:fun-def var parms body)
       (let ([new-parms (if (> (length parms) 4)
                            (begin
                              (addr-var-decl-list (cddddr parms) 0)
                              parms)
                            parms)]
             [new-body (addr-stmt body 0)])
         (ir:fun-def var new-parms new-body))]
      [else decl]))
  (define (addr-stmt stmt ofs)
    (match stmt
      [(ir:cmpd-stmt decls stmts)
       (let* ([ret (addr-var-decl-list decls ofs)]
              [new-decls (car ret)]
              [new-ofs (cdr ret)]
              [new-stmts (map (lambda (s)
                                      (addr-stmt s new-ofs))
                              stmts)])
         (ir:cmpd-stmt new-decls new-stmts))]
      [else stmt]))
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
                  (eq? 'array (car type)))
             (let* ([num (caddr type)]
                    [new-ofs (- ofs (* offset num))])
               (ett:set-decl-offset! obj (+ new-ofs offset))
               (cons var-decl new-ofs))]
            [else
             (ett:set-decl-offset! obj ofs)
             (cons var-decl (- ofs offset))])))
  (map addr-decl ir))
