#lang racket/base

(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(DrPython/lang/configure-runtime configure #f))]
      [else
       default])))
(provide get-info)