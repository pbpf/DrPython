#lang racket
(require parser-tools/lex)
(provide (all-defined-out))
;
(struct srcloc(start end) #:prefab)
(struct funcdef srcloc (name parameters suite test)#:prefab)
(struct classdef (name arglist body)#:prefab)
(struct dict (item)#:prefab)
(struct binary (op left right)#:prefab)
(struct unary (op self)#:prefab)
(struct suite(stmts)#:prefab)
(struct return_stmt srcloc(value)#:prefab)
(struct power (base index)#:prefab)
(struct assign_stmt srcloc (lst)#:prefab)
(struct augassign_stmt srcloc (left op right)#:prefab)
(struct definition srcloc (left right)#:prefab)
(struct name(refer)#:prefab)
(struct trailers (lst)#:prefab)
(struct call(parameters)#:prefab)
(struct pos (name)#:prefab)
(struct seq (name)#:prefab)
;(struct dict (name)#:prefab)
(struct subscript (slice) #:prefab)
(struct slice (start end step)#:prefab)
(struct multisubscript (lsts) #:prefab)

(struct pylist(lst)#:prefab)
(struct str(item)#:prefab)
(struct testbody (test body) #:prefab)
(struct att (name)#:prefab)
(struct argument_key (key value)#:prefab)
;;;
(struct pytuple-testlist_comp (base)#:prefab)
(struct for_stmt srcloc (expr seq body elsebody)#:prefab)
(struct if_stmt srcloc(test:bodylst elsebody)#:prefab)
(struct while_stmt srcloc (test loop elsebody)#:prefab)
(struct nonlocal_stmt srcloc (namelst)#:prefab)
(struct global_stmt srcloc (namelst)#:prefab)


(define(suite-append s lst)
  (suite (append (suite-stmts s) lst)))
(define(suite-append* s lst)
  (if s
      (suite (append (suite-stmts s) lst))
      (suite lst)))

(define(get-srcloc start-pos end-pos)
  (make-srcloc (file-path)
              (and start-pos (position-line start-pos))
              (and start-pos (position-col start-pos))
              (and start-pos (position-offset start-pos))
              (and start-pos (- (position-offset end-pos)
                                (position-offset start-pos)))))
(define(get-srcloc* s)
  (get-srcloc (srcloc-start s) (srcloc-end s)))


  
      