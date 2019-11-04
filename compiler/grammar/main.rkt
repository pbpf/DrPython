#lang racket/base
(require   parser-tools/lex
           parser-tools/yacc
           "lexer.rkt"
           "parser.rkt"
           "tools.rkt"
           )
(provide py-parser py-cmd-parser py-eval-parser)

;(define file-path (make-parameter #f))
(define get-token-grammar (make-lexer))

(define ((mk-parser which) ip)
  (define (go)

    
    (port-count-lines! ip)
    (define ip1 (reencode (skip-bom ip)))
    (which (lambda () (get-token-grammar ip1))))
  (if (file-path)
      (go)
      (parameterize ([file-path (object-name ip)]
                     [file-path (object-name ip)])
        (go))))
(define ((mk-parser-else which) ip)
  (define (go)
    (which (lambda () (get-token-grammar ip))))
        (go))

(define py-parser (mk-parser file-parser))
(define py-cmd-parser (mk-parser-else single-parser))
(define py-eval-parser (mk-parser-else eval-parser))
(module+ test
  (py-cmd-parser (open-input-string
"if 1>0:
     x=1
     y=5
else:
     x=2"))
  )
#|(funcdef
 (position 5 #f #f)
 (position 34 #f #f)
 'f
 '(#s(pos "x") #s(pos "y") #s(seq "z") #s(dict "l"))
 (suite
  (list (return_stmt (position 22 #f #f) (position 34 #f #f) '#s(binary + #s(binary + #s(name x) #s(name y)) (int . 1)))))
 #f)
|#