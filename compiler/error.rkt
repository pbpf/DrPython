#lang racket
(provide (all-defined-out))
;
(define-struct (exn:fail:python-syntax
                exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:python-syntax
         (msg marks a-srcloc))
       (list a-srcloc)])))