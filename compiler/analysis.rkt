#lang racket

(require "grammar/ast.rkt"
         "error.rkt"
         (prefix-in info: "ast.rkt")
         parser-tools/lex
         )
;
#|
首先扫描global 和nonlocal 声明 他们提到的变量在对应作用域中被视为可变,函数引用的外部变量也视为可变。否则视为不可变。

|#

(provide analysis-stmts analysis-stmt analysis-expr)
;------------------------------------------------------------------
(define current-optimization-level (make-parameter 1))

(define(analysis-parameter p)
  (match p
    [(pos name)(info:pos (string->symbol name))]
    [(seq  name)(info:seq (string->symbol name))]
    [(dict name)(info:dict (string->symbol name))]
    ))
(define(get-parameter-name p)
  (match p
    [(info:pos name) name]
    [(info:seq  name)name]
    [(info:dict name)name]))

(define(get-name p)
  (match p
    [(att name) name]))

  #|
(assign_stmt
 (position 1 #f #f)
 (position 20 #f #f)
 '(#s(name x)
   #s(trailers
      (#s(name c)
       #s(att "r")
       #s(att "g")
       #s(call (#s(name x) #s(name y)))
       #s(att "z")
       #s(subscript-multi (((int . 1) #f #f) ((int . 7) #f #f)))))))
|#
(define(argument_keys->table lst)
  (for/hasheq([i (in-list lst)])
    (values (info:argument_key-name i)(info:argument_key-value i))))

(define(parameter-check? params)
  (define t (memf info:argument_key? params))
  (if t
    (for/and ([ i (in-list t)])
    (info:argument_key? i))
    #t))

(define(analysis-trailers rlst assignment-scope decl);;;--- =>ast funccall
  (cond
    [(null? rlst)(values '()'())]
    [else (define-values(a b)(analysis-trailers (cdr rlst) assignment-scope decl))
          (match (car rlst)
              [(att name)
               (values (info:att a name)
                       b)]
              [(subscript lst)(values (info:subscript a lst) b)]
              [(call params)(define-values(c d)(gather-expr-list params assignment-scope decl))
                            (unless (parameter-check? c)
                              (error 'analysis-call "keyword args before pos args"))
                            (define-values(kwc1 c2)(partition info:argument_key? c))
                            (define c1 (argument_keys->table kwc1))
                            (match (cadr rlst)
                              [(att e)
                               (define-values(a1 b1)(analysis-trailers (cddr rlst) assignment-scope decl))
                               (values (info:call e
                                                  (cons a1 c2) c1)
                                        (set-union b1 d))]
                              [else (values (info:call a
                                                  c2 c1)
                                       d)])]
              [(name refer)(define-values(c d)(analysis-expr (car rlst) assignment-scope decl))
                          
                           (if(null? a)
                             (values c(set-union b d))
                             (error 'analysis-trailers "name must be first"))])]))

(define(analysis-expr expr assignment-scope decl);;;;=> ast and use-scope(father)
  (match expr
    [(list x ...)(let-values([(ast1 s1)(gather-expr-list x assignment-scope decl)])(values   ast1  s1))]
    [(cons type x) (values (info:constant type x) '())]
    [(name x)   (values (info:variable  x (set-member? assignment-scope x))
                        (if(or (set-member? assignment-scope x) (set-member?  decl x))
                            '()
                           (list x)))]
    [(trailers tlst)(analysis-trailers (reverse tlst)assignment-scope decl)]
    [(power base index) (let-values([(ast1 scope1)(analysis-expr base assignment-scope decl)]
                                    [(ast2 scope2)(analysis-expr index assignment-scope decl)])
                          (values (info:power ast1 ast2) (set-union scope1 scope2)))]
    [(pylist lst) (let-values([(ast1 s1)(gather-expr-list lst assignment-scope decl)])(values (info:pylist ast1) s1))]
    [(unary  op self) (let-values([(a s)(analysis-expr self assignment-scope decl)])(values (info:unary op a) s))]
    [(binary op left right) (let-values([(a1 s1)(analysis-expr left assignment-scope decl)]
                                        [(a2 s2)(analysis-expr right assignment-scope decl)])
                              (values(info:binary op a1 a2)(set-union s1 s2)))]
    [(pytuple-testlist_comp base)(analysis-expr base assignment-scope decl)]
    [(argument_key id v)(define-values(a b)(analysis-expr v assignment-scope decl))
                            (match id
                              [(name k)(values (info:argument_key k a) b)]
                              [else (error 'analysis-expr-argument_key "~a is not id" id)])]
    [_ (error 'analysis-expr "~a: is not a expr" expr)]))

(define(gather-expr-list lst assignment-scope decl)
  (for/fold ([lst '()]
             [s '()]
             )
            ([i (in-list lst)])
    (let-values([(a1 s1)(analysis-expr i assignment-scope decl)])
      (values (append lst(list a1))
              (set-union s s1)))))
;-----------------------------------

;--------------leftexpr----------------------
(define(analysis-leftexpr leftexpr  assignment-scope decl)
  (match leftexpr
    [(name x)(if(set-member? decl x)
                (values (info:topvariable decl)  assignment-scope)
                (values (info:variable x (set-member?  assignment-scope x))
                     (set-add assignment-scope x)))]
    [(list x ...)(for/fold ([asts '()]
                            [s assignment-scope])
                           ([i (in-list x)])
                           (let-values([(a1 s1)(analysis-leftexpr i s decl)])
                             (values (append asts (list a1))
                                     s1)))]))
                   
;-----------------------------------
(define(analysis-test:bodylst tlst assignment-scope )
  (for/list ([i (in-list tlst)])
     (analysis-stmt i assignment-scope)))

(define(raise-variable-error lst loc)
  (when (not (null? lst))
    (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is assign or use to before global or nonlocal declaration" (string-join (map symbol->string lst)))
                                   (current-continuation-marks)
                                   loc))))
(define(analysis-stmts stmts [assignment-scope '()])
  (for/fold([asts '()]
            [as assignment-scope]
            [us '()]
            [decl '()]
            )
           ([i (in-list stmts)])
    (let-values([(asts1 as1 us1 decl1)(analysis-stmt i as us decl)])
      ;(raise-variable-error(set-intersect (set-union assglo assnono)(set-union us (hash-keys as))) (get-srcloc* i))
      (values (append asts asts1)
              as1
              (set-union us us1)
              (append decl decl1)
              ))))
;-------------------------------------------------------
(define(analysis-suite su assignment-scope)
  (define-values(astlst asr usr decl) (analysis-stmts (suite-stmts su) assignment-scope))
  (values  (info:suite astlst asr usr decl) asr usr decl))
(define(analysis-testbody tb assignment-scope decl)
  (let-values([(ast us)(analysis-expr (testbody-test tb) assignment-scope decl)]
              [(ast1 as1 us1 decl1)(analysis-suite (testbody-body tb)assignment-scope)])
    (values  (info:testbody ast ast1)
            as1
            (set-union us us1)
            decl1
           )))

;;;-----------------------------------------------------------
;;;
(define(collect-func-params lst)
  (for/fold([posr '()]
            [seqr #f]
            [dictr #f])
           ([i (in-list lst)])
    (if(info:pos? i)
       (values (append posr (list (info:pos-name i)))
               seqr dictr)
       (if(info:seq? i)
          (values posr (info:seq-name i) dictr)
          (values posr seqr (info:dict-name i))))))
(define(analysis-stmt stmt assignment-scope [us '()][decl '()])
  (match stmt
    [(assign_stmt start end lst)(let ([rlst (reverse lst)])
                                  (define-values (a1 us)(analysis-expr (car rlst) assignment-scope decl))
                                  (define-values (rasts ss)
                                    (for/fold ([asts '()]
                                               [as assignment-scope])
                                              ([lefti (in-list (cdr rlst))])
                                      (let-values([(a as)(analysis-leftexpr lefti as decl)])
                                        (values (cons a asts) as))))
                                  (define asts (reverse rasts))
                                  (values (for/fold ([t (info:make-assign_stmts (position-offset start) (car asts)  a1)])
                                                    ([i (in-list (cdr asts))])
                                            (append t (info:make-assign_stmts* i (car asts))))
                                          ss us '()))]           
    [(if_stmt start end test:bodylst elsebody)(define-values(easts eas eus decl1)(analysis-suite   elsebody  assignment-scope));->4
                                              (define-values (ifrasts ifuse  ifnewdef)
                                                (for/fold ([asts '()]
                                                           [uses eus]
                                                           [newdef (set-subtract  eas assignment-scope)]
                                                           )
                                                          ([i (in-list test:bodylst)])
                                                (let-values([(ast1 as1 us1 decl1)(analysis-testbody   i assignment-scope decl)])
                                                  (values (cons ast1 asts)
                                                          (set-union uses us1)
                                                          (set-intersect newdef (set-subtract as1 assignment-scope))))))
                                              (define assignment-scope2 (set-union assignment-scope ifnewdef))
                                              (define-values(easts2 eas2 eus2 decl12)(analysis-suite   elsebody  assignment-scope2))
                                              
                                              (define iftestbodylst (map (lambda(x)(call-with-values (lambda()(analysis-testbody x assignment-scope2 decl))
                                                                                           (lambda(a b c d) a)))
                                                                         test:bodylst))
                                              (values `( ,@(map (lambda(x)(info:undef x))ifnewdef) ,(info:if_stmt iftestbodylst easts2))
                                                      assignment-scope2
                                                      ifuse '())]
    [(funcdef start end name parameters suite1 test)(define params (map analysis-parameter parameters))
                                                    (define param-names (map get-parameter-name params))
                                                    (define-values (s as us decl)(analysis-suite suite1  param-names))
                                                    (define-values (p1 s1 d1)(collect-func-params params))
                                                    (values (list (info:funcdef name p1 s1 d1 s test assignment-scope))
                                                          assignment-scope
                                                          '()'())]
    [(global_stmt start end namelst)(define usbeforeglo (set-intersect us namelst))
                                    (define assbeforeglo (set-intersect  assignment-scope  namelst))
                                    (define declbeforeglo (set-intersect decl namelst))
                                    (if(null? usbeforeglo)
                                       (if(null?  assbeforeglo)
                                          (if(null? declbeforeglo)
                                             (values (list(info:global_stmt namelst))
                                                     assignment-scope
                                                     '()
                                                     namelst)
                                             (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is decled before global declaration"
                                                                                 (string-join (map symbol->string assbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))
                                          (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is assigned before global declaration"
                                                                                 (string-join (map symbol->string assbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))
                                       (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is used before global declaration"
                                                                                 (string-join (map symbol->string usbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))]
    [(nonlocal_stmt start end namelst)(define usbeforeglo (set-intersect us namelst))
                                    (define assbeforeglo (set-intersect   assignment-scope  namelst))
                                    (define declbeforeglo (set-intersect decl namelst))
                                    (if(null? usbeforeglo)
                                       (if(null?  assbeforeglo)
                                          (if(null? declbeforeglo)
                                             (values (list(info:global_stmt namelst))
                                                     assignment-scope
                                                     '()
                                                     namelst)
                                             (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is decled before nonlocal declaration"
                                                                                 (string-join (map symbol->string assbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))
                                          (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is assigned before nonlocal declaration"
                                                                                 (string-join (map symbol->string assbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))
                                       (raise (exn:fail:python-syntax (format "SyntaxError: name ~a is used before nonlocal declaration"
                                                                                 (string-join (map symbol->string usbeforeglo)))
                                                                         (current-continuation-marks)
                                                                         (get-srcloc start end))))]
                                                                      
    [(return_stmt start end  value)(define-values(ast us)(analysis-expr value assignment-scope decl))
                   (values (list(info:return_stmt ast))
                           assignment-scope
                           us '())]
    [(trailers tlst)(define-values (ast us)(analysis-expr (trailers tlst) assignment-scope decl))
                    (values (list  ast)
                           assignment-scope
                           us '())]
                    
    [else (error 'analysis-stmt "~a is not a stmt" stmt)]))
                                                            
     

(module+ test
 
(analysis-stmt (if_stmt
 (position 1 #f #f)
 (position 31 #f #f)
 (list
  (testbody
   '#s(binary > (int . 1) (int . 0))
   (suite
    (list
     (assign_stmt (position 14 #f #f) (position 17 #f #f) '(#s(name x) (int . 1)))
     (assign_stmt (position 23 #f #f) (position 26 #f #f) '(#s(name y) (int . 5)))))))
 (suite (list (assign_stmt (position 38 #f #f) (position 41 #f #f) '(#s(name x) (int . 2)))))) '() '()))
