#lang racket
;
(provide (all-defined-out))
;expr
(struct expr (refs)#:prefab)
(struct constant (type value)#:prefab)
(struct tmpid(id)#:prefab)
(struct variable (name tag)#:prefab)
(struct topvariable (name)#:prefab)
(struct binary (op left right)#:prefab)
(struct unary (op self)#:prefab)
(struct power (base index)#:prefab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct funcdef (name posparameters seq dict suite test father_scope)#:prefab)
(struct dict (name)#:prefab)
(struct suite(stmts as us decl)#:prefab)
;(struct suite_temp(stmts)#:prefab)
(struct return_stmt (value)#:prefab)
(struct assign_stmt (left right)#:prefab)
(struct assign_stmts (left right tag)#:prefab)
(struct augassign_stmt (left op right)#:prefab)
(struct definition (left right)#:prefab)
(struct name(refer)#:prefab)
(struct trailers (lst)#:prefab)
(struct call(func parameters keywordstable)#:prefab)
(struct pos (name)#:prefab)
(struct seq (name)#:prefab)
(struct att(base name)#:prefab)
(struct subscript (base lst) #:prefab)

(struct pylist(lst)#:prefab)
(struct str(item)#:prefab)
(struct testbody (test body) #:prefab)
(struct argument_key (name value)#:prefab)
(struct undef(name)#:prefab)
;;;
(struct pytuple-testlist_comp (base)#:prefab)
(struct for_stmt (expr seq body elsebody)#:prefab)
(struct if_stmt  (test:bodylst elsebody)#:prefab)
(struct nonlocal_stmt  (namelst)#:prefab)
(struct global_stmt  (namelst)#:prefab)

;--------------------------------------------------------

(define(make-tmp-assign_stmts ids a   b)
  (define x (for/list ([i (in-naturals ids)]
                       [j (in-list a)])
              (tmpid i)))
  (cons
   (assign_stmts x b #f)
   (for/list ([i (in-list a)]
              [j (in-list x)])
     (assign_stmt i j))))
  
(define(ssa? a)
  (and (variable? a)(not(variable-tag a))))
(define(ressa? a)
   (and (variable? a) (variable-tag a)))
(define(all-has? func? lst)
  (for/and ([i (in-list lst)])
    (func? i)))
(define(make-assign_stmts offset a b)
  (if(list? a)
     (if(all-has? ssa? a)
        (list (assign_stmts a b #f))
        (if (all-has? ressa? a)
            (list (assign_stmts a b #t))
            (make-tmp-assign_stmts offset  a  b)))
  (list(assign_stmt a b))))
(define(make-assign_stmts* a b)
  (if(list? a)
     (for/list ([i (in-list a)]
                [j (in-list b)])
       (assign_stmt i j))
  (list (assign_stmt a b))))
  
  