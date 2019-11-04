(module reader syntax/module-reader
  #:language 'DrPython
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t  
  #:language-info
  '#(DrPython/lang/lang-info get-info #f)
  #:info (lambda (key defval default)
           ; XXX Should have different comment character key
           (case key
             [(drracket:default-filters) '(["Python Sources" "*.py"])]
             [(drracket:default-extension) "py"]
             ;[(drracket:opt-out-toolbar-buttons)'(File Edit file edit)]
             [(color-lexer interaction-color-lexer)
              (dynamic-require 'DrPython/compiler/grammar/colorlexer 'colorizer-lexer)]
             [else (default key defval)]))
  

  (require  "../compiler/grammar/main.rkt"
            "../compiler/compiler.rkt"
            "../compiler/analysis.rkt"
            parser-tools/lex
           )
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (define t (parameterize ([file-path src])
       (py-parser in)))
    ;(write t)
    (define-values(a b c d)(analysis-stmts t))
    (compile-program a  )))