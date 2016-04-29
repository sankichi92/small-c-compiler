#lang racket
(require (prefix-in ett: "entity.rkt")
         (prefix-in ir:  "ir.rkt"))
(provide addr-assign)

(define (addr-assign ir)
  (define offset 4)
  (define (addr-decl decl)
    (match decl
      [(ir:fun-def var parms body)
       (let* ([new-parms (let ([len (length parms)]
                               [objs (map ir:var-decl-var parms)])
                           (map ett:set-decl-lev! objs (build-list len values))
                           (when (> len 4)
                                 (addr-var-decl-list (cddddr parms)))
                           parms)]
              [ret (addr-stmt body)]
              [new-body (car ret)]
              [size (- (cdr ret))])
         (cons (ir:fun-def var new-parms new-body) size))]
      [else decl]))
  (define (addr-stmt stmt [ofs 0])
    (match stmt
      [(ir:cmpd-stmt decls stmts)
       (let* ([ret-decls (addr-var-decl-list decls ofs)]
              [new-decls (car ret-decls)]
              [new-ofs (cdr ret-decls)]
              [max-ofs new-ofs]
              [new-stmts (map (lambda (s)
                                (let* ([ret (addr-stmt s new-ofs)]
                                       [new-stmt (car ret)]
                                       [new-max-ofs (cdr ret)])
                                  (when (< new-max-ofs max-ofs)
                                        (set! max-ofs new-max-ofs))
                                  new-stmt))
                              stmts)])
         (cons (ir:cmpd-stmt new-decls new-stmts) max-ofs))]
      [else (cons stmt ofs)]))
  (define (addr-var-decl-list var-decls [ofs 0])
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
