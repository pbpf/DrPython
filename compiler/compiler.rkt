#lang racket
(require "ast.rkt"
         "tools.rkt"
         parser-tools/lex)
(provide compile-statement compile-program compile-expr)
;muitable
(define(compile-leftexpr expr)
  (match expr
    [(variable name tag)(datum->syntax #f name)]
    [_ (error 'compile-leftexpr "~a: is not a leftexpr" expr)]))

(define(symbol->keyword s)
  (string->keyword (symbol->string s)))
(define(compile-kws kt)
  (apply append
         (for/list ([(k v)(in-hash kt)])
           (list (symbol->keyword k) (compile-expr v)))))
(define(compile-expr expr)
  (match expr
   [(constant type value)  (datum->syntax #f value) ]
   [(variable name tag)(datum->syntax #f name)]
   [(binary op left right)(datum->syntax #f `(,op ,(compile-expr left) ,(compile-expr right)))]
   [(unary op self)(datum->syntax #f `(,op ,(compile-expr self)))]
   [(power base index)(datum->syntax #f `(expt ,(compile-expr base),(compile-expr index)))]
   [(call name params dicts)(datum->syntax #f `(,(compile-expr name) ,@(map compile-expr params) ,@(compile-kws dicts)))]
   [_ (error 'compile-expr "~a: is not a expr" expr)]))


(define(suite-return-variable  tb x)
  (variable x (hash-ref (suite-as  tb) x)))
(define(table->variables tb)
  (for/list ([(k v)(in-hash tb)])
    (variable k v)))
(define(compile-suite s)
  (map compile-statement (suite-stmts s)))
(define(compile-statement stmt [variables '()])
  (match stmt
   [(assign_stmt left right) (define x (datum->syntax #f 'x))
                              (match left
                                [(variable name tag)
                                 (if tag
                                     (datum->syntax  x `(set! ,(compile-leftexpr left) ,(compile-expr right)))
                                     (datum->syntax  x `(define ,(compile-leftexpr left) ,(compile-expr right))))])]
    [(assign_stmts left right tag) (define x (datum->syntax #f 'x))
                                   (if tag
                                       (datum->syntax  x `(set-values! ,(map compile-leftexpr left) (values ,@(map compile-expr right))))
                                       (datum->syntax  x `(define-values ,(map compile-leftexpr left) (values ,@(map compile-expr right)))))]
  
    [(if_stmt  test:bodylst elsebody)(define x (datum->syntax #f 'x))
                                                                 
                                                                       (datum->syntax  x `(cond ,@(map (lambda(x)`(,(compile-expr (testbody-test x))
                                                                                                   ,@(compile-suite (testbody-body x))))
                                                                                       test:bodylst)
                                                                                (else ,@(compile-suite  elsebody))))]
    [(funcdef name posparameters seq dict suite test father_scope)
     (define x (datum->syntax #f 'x))
      (datum->syntax  x
                      `(define ,name (make-keyword-procedure (lambda(kws kw-args . rest)
                                                               (define-values (vtb  seq1 dict1)
                                                                 (for/fold ([tb '()]
                                                                            [restr rest]
                                                                            [kwt (for/hash ([@i (in-list kws)]
                                                                                            [@j (in-list kw-args)])
                                                                                   (values (string->symbol(keyword->string @i)) @j))])
                                                                            ([i (in-list ',posparameters)])
                                                                            (if(null? restr)
                                                                               (values (cons  (hash-ref kwt i (lambda()(error ',name "function parameter ~a has no value" i))) tb)
                                                                                       '()  (hash-remove  kwt i))
                                                                               (values (cons (car restr) tb) (cdr restr) kwt))))
                                                               (match-define (list ,@posparameters) (reverse vtb))
                                                               ,(if seq `(define ,seq seq1) `(unless (null? seq1) (error ',name "can not accpet parameters ~a" seq1)))
                                                               ,(if dict `(define ,dict dict1) `(unless (null? (hash-keys dict1)) (error ',name "can not accpet parameters ~a" dict1)))
                                                               (let/cc @return
                                                               ,@(compile-suite suite))))))]
    [(return_stmt value)(datum->syntax #f `(@return ,(compile-expr value)))]
    [(undef name)(define x (datum->syntax #f 'x)) (datum->syntax  x `(define ,name undefined))]
    [(call name params dicts)(datum->syntax #f `(,(compile-expr name) ,@(map compile-expr params) ,@(compile-kws dicts)))]
                                
                                                                                     
                                                                                                     
                                                                     
    ;[(call parameters)#`(lambda(&f)(&f  #,@(map compile-statement parameters)))]
   ; [(trailers lst) #`(call-in-order #,(compile-statement(car lst))#,(cons 'list (map compile-statement (cdr lst))))]
    ;[(pylist lst)#`(vector #,@(map compile-statement lst))]
    [_ (error 'compile-statement "unknown statement:~a" stmt)]))

(define(compile-program lst)
  (map compile-statement lst))

(module+ test
  (compile-statement
   #s(if_stmt
     (#s(testbody
         #s(binary > #s(constant int 1) #s(constant int 0))
         #s(suite
            (#s(assign_stmt #s(variable x #t) #s(constant int 1)) #s(assign_stmt #s(variable y #f) #s(constant int 5)))
            (y x)
            ()
            ())))
     #s(suite (#s(assign_stmt #s(variable x #t) #s(constant int 2))) (x) () ())
    )))