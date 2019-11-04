#lang racket/base
(require parser-tools/lex)

(provide (all-defined-out))

(define-tokens tokens (DEDENT INDENT NAME  NUMBER STRING BYTES))
(define-empty-tokens empty-tokens
    (ENDMARKER @ -> nonlocal with ... None True False NEWLINE 
     != % %= & &= |(| |)| * ** **= *= + += |,| - -= |.| / // //= /= : |;|
     < << <<= <= <> := = == > >= >> >>= |[| |]| ^ ^= |`| and as assert break class 
     continue def del elif else except exec finally for from global if import  
     in is lambda not or pass print raise return try while yield |{| \| \|= |}| ~ EOF))