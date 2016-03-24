#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         )
(define ... (void)) ;; indicates a part to be implemented


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
     ((external-declaration) ...)
     ((program external-declaration) ...))
    (external-declaration
     ((declaration) ...)
     ((function-prototype) ...)
     ((function-definition) ...))
    (declaration
     ((type-specifier declarator-list SEMI) ...))
    (declarator-list
     ((declarator) ...)
     ((declarator-list COMMA declarator) ...))
    (declarator
     ((direct-declarator) ...)
     ((* direct-declarator) ...))
    (direct-declarator
     ((ID) ...)
     ((ID LBBRA NUM RBBRA) ...))
    (function-prototype
     ((type-specifier function-declarator SEMI) ...))
    (function-declarator
     ((ID LPAR parameter-type-list-opt RPAR) ...)
     ((* ID LPAR parameter-type-list-opt RPAR) ...))
    (function-definition
     ((type-specifier function-declarator compound-statement) ...))
    (parameter-type-list-opt
     (() ...)
     ((parameter-type-list) ...))
    (parameter-type-list
     ((parameter-declaration) ...)
     ((parameter-type-list COMMA parameter-declaration) ...))
    (parameter-declaration
     ((type-specifier parameter-declarator) ...))
    (parameter-declarator
     ((ID) ...)
     ((* ID) ...))
    (type-specifier
     ((INT) ...)
     ((VOID) ... ))
    (statement
     ((SEMI) ...)
     ((expression SEMI) ...)
     ((compound-statement) ...)
     ((IF LPAR expression RPAR statement) ...)
     ((IF LPAR expression RPAR statement ELSE statement) ...)
     ((WHILE LPAR expression RPAR statement) ...)
     ((FOR LPAR expression-opt SEMI expression-opt
           SEMI expression-opt RPAR statement) ...)
     ((RETURN expression-opt SEMI) ...))
    (compound-statement
     ((LBRA declaration-list-opt statement-list-opt RBRA) ...))
    (declaration-list-opt
     (() ...)
     ((declaration-list) ...))
    (declaration-list
     ((declaration) ...)
     ((declaration-list declaration) ...))
    (statement-list-opt
     (() ...)
     ((statement-list) ...))
    (statement-list
     ((statement) ...)
     ((statement-list statement) ...))
    (expression-opt
     (() ...)
     ((expression) ...))
    (expression
     ((assign-expr) ...)
     ((expression COMMA assign-expr) ...))
    (assign-expr
     ((logical-or-expr) ...)
     ((logical-or-expr = assign-expr) ...))
    (logical-or-expr
     ((logical-and-expr) ...)
     ((logical-or-expr || logical-and-expr) ...))
    (logical-and-expr
     ((equality-expr) ...)
     ((logical-and-expr && equality-expr) ...))
    (equality-expr
     ((relational-expr) ...)
     ((equality-expr == relational-expr) ...)
     ((equality-expr != relational-expr) ...))
    (relational-expr
     ((add-expr) ...)
     ((relational-expr < add-expr) ...)
     ((relational-expr > add-expr) ...)
     ((relational-expr <= add-expr) ...)
     ((relational-expr >= add-expr) ...))
    (add-expr
     ((mult-expr) ...)
     ((add-expr + mult-expr) ...)
     ((add-expr - mult-expr) ...))
    (mult-expr
     ((unary-expr) ...)
     ((mult-expr * unary-expr) ...)
     ((mult-expr / unary-expr) ...))
    (unary-expr
     ((postfix-expr) ...)
     ((- unary-expr) ...)
     ((& unary-expr) ...)
     ((* unary-expr) ...))
    (postfix-expr
     ((primary-expr) ...)
     ((postfix-expr LBBRA expression RBBRA) ...)
     ((ID LPAR argument-expression-list-opt RPAR) ...))
    (primary-expr
     ((ID) ...)
     ((NUM) ...)
     ((LPAR expression RPAR) ...))
    (argument-expression-list-opt
     (() ...)
     ((argument-expression-list) ...))
    (argument-expression-list
     ((assign-expr) ...)
     ((argument-expression-list COMMA assign-expr) ...)))))
