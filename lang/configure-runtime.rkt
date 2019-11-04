#lang racket/base

(define (configure data)
  (current-read-interaction the-read))

(provide configure)

(require "../compiler/grammar/main.rkt"
         "../compiler/compiler.rkt"
         "../compiler/analysis.rkt"
         parser-tools/lex)

(define (the-read src ip)
  (cond
    [(or (not (char-ready? ip))(eof-object? (peek-char ip))) eof]
    [else (define t (parameterize ([file-path src])
         (py-cmd-parser ip)))
          (define-values(a b)(analysis-expr t '() '()))
          (compile-expr a)]))
