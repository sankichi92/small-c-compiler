#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (prefix-in stx: "syntax.rkt"))
(provide parse-string parse-file)

(define-tokens tokens-with-value
  (NUM ID))

(define-empty-tokens tokens-without-value
  (+ - * /
   < <= > >= == !=
   & && || =
   SEMI LPAR RPAR COMMA RETURN
   LBRA RBRA LBBRA RBBRA
   INT VOID
   IF ELSE WHILE FOR
   EOF))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:* d))))

(define-lex-abbrevs
  (digit            (char-range "0" "9"))
  (digit-non-zero   (char-range "1" "9"))
  (number  (:or "0"
                (:: digit-non-zero
                    (uinteger digit))))
  (identifier-char (:or (char-range "a" "z")
                        (char-range "A" "Z")))
  (identifier (:: identifier-char
                  (:* (:or identifier-char digit "_")))))

(define small-c-lexer
  (lexer-src-pos
   ("+"        (token-+))
   ("-"        (token--))
   ("*"        (token-*))
   ("/"        (token-/))
   ("<"        (token-<))
   ("<="       (token-<=))
   (">"        (token->))
   (">="       (token->=))
   ("=="       (token-==))
   ("!="       (token-!=))
   ("&"        (token-&))
   ("&&"       (token-&&))
   ("||"       (token-||))
   ("="        (token-=))
   (";"        (token-SEMI))
   ("("        (token-LPAR))
   (")"        (token-RPAR))
   ("{"        (token-LBRA))
   ("}"        (token-RBRA))
   ("["        (token-LBBRA))
   ("]"        (token-RBBRA))
   (","        (token-COMMA))
   ("return"   (token-RETURN))
   ("if"       (token-IF))
   ("else"     (token-ELSE))
   ("while"    (token-WHILE))
   ("for"      (token-FOR))
   ("int"      (token-INT))
   ("void"     (token-VOID))
   (number     (token-NUM (string->number lexeme)))
   (identifier (token-ID (string->symbol lexeme)))
   (whitespace (return-without-pos (small-c-lexer input-port)))
   ((eof)      (token-EOF))))

(define small-c-parser
  (parser
   (start program)
   (end EOF)
   (src-pos)
   ;;(debug "small-c-parser.tbl")
   (suppress)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error (format "parse error:~a,~a: ~a"
                           (position-line start-pos)
                           (position-col start-pos)
                           (if tok-value tok-value tok-name)))))
   (tokens tokens-with-value tokens-without-value)
   (grammar
    (program
     ((external-declaration) (list $1))
     ((program external-declaration) `(,@$1 ,$2)))
    (external-declaration
     ((declaration) $1)
     ((function-prototype) $1)
     ((function-definition) $1))
    (declaration
     ((type-specifier declarator-list SEMI) (stx:var-decls $1 $2 $1-start-pos)))
    (declarator-list
     ((declarator) (list $1))
     ((declarator-list COMMA declarator) `(,@$1 ,$3)))
    (declarator
     ((direct-declarator) (stx:var-decl (car $1) (cdr $1) $1-start-pos))
     ((* direct-declarator) (stx:var-decl (car $2) `(pointer ,(cdr $2)) $1-start-pos)))
    (direct-declarator
     ((ID) (cons $1 '()))
     ((ID LBBRA NUM RBBRA) (cons $1 (cons 'array $3))))
    (function-prototype
     ((type-specifier function-declarator SEMI)
      (stx:fun-decl (car $2) (format-type $1 (cadr $2)) (map stx:parm-decl-ty (caddr $2)) $1-start-pos)))
    (function-declarator
     ((ID LPAR parameter-type-list-opt RPAR) (list $1 '() $3))
     ((* ID LPAR parameter-type-list-opt RPAR) (list $2 'pointer $4)))
    (function-definition
     ((type-specifier function-declarator compound-statement)
      (stx:fun-def (car $2) (format-type $1 (cadr $2)) (caddr $2) $3 $1-start-pos)))
    (parameter-type-list-opt
     (() '())
     ((parameter-type-list) $1))
    (parameter-type-list
     ((parameter-declaration) (list $1))
     ((parameter-type-list COMMA parameter-declaration) `(,@$1 ,$3)))
    (parameter-declaration
     ((type-specifier parameter-declarator)
      (stx:parm-decl (car $2) (format-type $1 (cdr $2)) $1-start-pos)))
    (parameter-declarator
     ((ID) (cons $1 '()))
     ((* ID) (cons $2 'pointer)))
    (type-specifier
     ((INT) 'int)
     ((VOID) 'void))
    (statement
     ((SEMI) '())
     ((expression SEMI) $1)
     ((compound-statement) $1)
     ;((IF LPAR expression RPAR statement) (stx:if-stmt $3 $5 $1-start-pos))
     ((IF LPAR expression RPAR statement) (stx:if-els-stmt $3 $5 '() $1-start-pos))
     ((IF LPAR expression RPAR statement ELSE statement) (stx:if-els-stmt $3 $5 $7 $1-start-pos))
     ((WHILE LPAR expression RPAR statement) (stx:while-stmt $3 $5 $1-start-pos))
     ;((FOR LPAR expression-opt SEMI expression-opt SEMI expression-opt RPAR statement)
     ; (stx:for-stmt $3 $5 $7 $9 $1-start-pos))
     ((FOR LPAR expression-opt SEMI expression-opt SEMI expression-opt RPAR statement)
      `(,@$3 ,(stx:while-stmt $5 (stx:cmpd-stmt '() (list $9 $7) '()) $1-start-pos)))
     ((RETURN expression-opt SEMI) (stx:ret-stmt $2 $1-start-pos)))
    (compound-statement
     ((LBRA declaration-list-opt statement-list-opt RBRA) (stx:cmpd-stmt $2 $3 $1-start-pos)))
    (declaration-list-opt
     (() '())
     ((declaration-list) $1))
    (declaration-list
     ((declaration) (list $1))
     ((declaration-list declaration) `(,@$1 ,$2)))
    (statement-list-opt
     (() '())
     ((statement-list) $1))
    (statement-list
     ((statement) (list $1))
     ((statement-list statement) `(,@$1 ,$2)))
    (expression-opt
     (() '())
     ((expression) $1))
    (expression
     ((assign-expr) (list $1))
     ((expression COMMA assign-expr) `(,@$1 ,$3)))
    (assign-expr
     ((logical-or-expr) $1)
     ((logical-or-expr = assign-expr) (stx:assign-exp $1 $3 $2-start-pos)))
    (logical-or-expr
     ((logical-and-expr) $1)
     ((logical-or-expr || logical-and-expr) (stx:lop-exp '|| $1 $3 $2-start-pos)))
    (logical-and-expr
     ((equality-expr) $1)
     ((logical-and-expr && equality-expr) (stx:lop-exp '&& $1 $3 $2-start-pos)))
    (equality-expr
     ((relational-expr) $1)
     ((equality-expr == relational-expr) (stx:rop-exp '== $1 $3 $2-start-pos))
     ((equality-expr != relational-expr) (stx:rop-exp '!= $1 $3 $2-start-pos)))
    (relational-expr
     ((add-expr) $1)
     ((relational-expr < add-expr) (stx:rop-exp '< $1 $3 $2-start-pos))
     ((relational-expr > add-expr) (stx:rop-exp '> $1 $3 $2-start-pos))
     ((relational-expr <= add-expr) (stx:rop-exp '<= $1 $3 $2-start-pos))
     ((relational-expr >= add-expr) (stx:rop-exp '>= $1 $3 $2-start-pos)))
    (add-expr
     ((mult-expr) $1)
     ((add-expr + mult-expr) (stx:aop-exp '+ $1 $3 $2-start-pos))
     ((add-expr - mult-expr) (stx:aop-exp '- $1 $3 $2-start-pos)))
    (mult-expr
     ((unary-expr) $1)
     ((mult-expr * unary-expr) (stx:aop-exp '* $1 $3 $2-start-pos))
     ((mult-expr / unary-expr) (stx:aop-exp '/ $1 $3 $2-start-pos)))
    (unary-expr
     ((postfix-expr) $1)
     ;((- unary-expr) (stx:neg-exp $2 $1-start-pos))
     ((- unary-expr) (stx:aop-exp '- (stx:lit-exp 0 '()) $2 $1-start-pos))
     ((& unary-expr) (stx:addr-exp $2 $1-start-pos))
     ((* unary-expr) (stx:deref-exp $2 $1-start-pos)))
    (postfix-expr
     ((primary-expr) $1)
     ;((postfix-expr LBBRA expression RBBRA) (stx:arr-exp $1 $3 $2-start-pos))
     ((postfix-expr LBBRA expression RBBRA) (stx:deref-exp (list (stx:aop-exp '+ $1 (car $3) '())) $2-start-pos))
     ((ID LPAR argument-expression-list-opt RPAR) (stx:fun-exp $1 $3 $1-start-pos)))
    (primary-expr
     ((ID) (stx:var-exp $1 $1-start-pos))
     ((NUM) (stx:lit-exp $1 $1-start-pos))
     ((LPAR expression RPAR) $2))
    (argument-expression-list-opt
     (() '())
     ((argument-expression-list) $1))
    (argument-expression-list
     ((assign-expr) (list $1))
     ((argument-expression-list COMMA assign-expr) `(,@$1 ,$3))))))

(define (format-type ty pt)
  (if (null? pt)
    ty
    (list pt ty)))

(define (add-print-fun ast)
  (append
    (list
      (stx:fun-decl
        'print
        'void
        (list 'int)
        '()))
    ast))

(define (parse-port port)
  (port-count-lines! port)
  (add-print-fun
    (small-c-parser (lambda () (small-c-lexer port)))))

(define (parse-string str)
  (parse-port (open-input-string str)))

(define (parse-file fname)
  (parse-port (open-input-file fname)))
