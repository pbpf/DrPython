#lang racket
(require parser-tools/lex
         "abbrevs.rkt"
         (prefix-in : parser-tools/lex-sre))

(provide colorizer-lexer color-token)

  (define colorizer-lexer
    (lexer
     ;; whitespace
     [(:+ (:or nu-line py-blank)) (color-token 'white-space)]
     ;; comments
     [comment (color-token 'comment)]
     ;; backslash
     [#\\ (color-token 'other)]
     ;; keywords (def, return, if, etc.)
     [special-keyword (color-token 'hash-colon-keyword)]
     [(:or binary-operator misc-operator) (color-token 'other)]
     ;; parenthesis
     [(:or "(" ")" "[" "]" "{" "}") (color-token 'parenthesis (string->symbol lexeme))]
     ;; identifiers
     [(:: (:or letter "_") (:* (:or letter digit "_"))) (color-token 'symbol)]
     ;; string literals
     [stringliteral (color-token 'string)]
     ;; number literals
     [(:: (:or decimal-integer oct-integer hex-integer)
          (:? (:or #\l "L")))
      (color-token 'constant)]
     [float (color-token 'constant)]
     [(:: (:or float int-part) (:or #\j "J")) (color-token 'constant)]
     ;; incomplete string literals
     [(:: string-prefix "'" (:* short-string-item1))
      (color-token 'error)]
     [(:: string-prefix #\" (:* short-string-item2))
      (color-token 'error)]
     [(:: string-prefix "'''" (:* (:or long-string-item1
                                       (:: #\' long-string-item1)
                                       (:: "''" long-string-item1))))
      (color-token 'error)]
     [(:: string-prefix "\"\"\"" (:* (:or long-string-item2
                                          (:: #\" long-string-item2)
                                          (:: "\"\"" long-string-item2))))
      (color-token 'error)]
     ;; misc
     [(eof) (values lexeme 'eof #f #f #f)]
     [(special) (color-token 'error)]
     [(special-comment) (color-token 'error)]
     [any-char (color-token 'error)]))

 (define-syntax (color-token stx)
    (syntax-case stx ()
      [(_ category)
       #'(color-token category #f)]
      [(_ category paren)
       (with-syntax ([lexeme    (datum->syntax stx 'lexeme)]
                     [start-pos (datum->syntax stx 'start-pos)]
                     [end-pos   (datum->syntax stx 'end-pos)])
         #'(values lexeme category paren (position-offset start-pos) (position-offset end-pos)))]))