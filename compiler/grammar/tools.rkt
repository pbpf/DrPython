#lang racket
(require parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           syntax/readerr
           "token.rkt"
           "abbrevs.rkt"
           )

(provide(all-defined-out))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(skip-bom ip)
  (define x (peek-bytes 3 0 ip))
  (when(and (bytes? x)(bytes=? x #"\357\273\277"))
     (read-bytes 3 ip))
     ip)
(define (get-encode-from-string str)
  (define x(regexp-match #px"(?<=coding:)([\\s]*)?[\\S]*" str))
  (if x (substring (car x) (string-length (cadr x))) ""))

(define (get-encode ip)
  (let loop ([jmp 0])
    (define c (peek-byte ip jmp))
    (cond
      [(and (number? c)(= c 35)) (let-values([(str jp)(peek-line ip jmp)])
                  (define enc (get-encode-from-string str))
                  (if (string=? enc "")
                      (loop jp)
                      enc))]
      [else ""])))
           
  
(define(peek-line ip jmp)
  (let loop([collec '()][i (+ jmp 1)][char(peek-char-or-special ip jmp)])
    (if(or (eof-object? char)(char=? char #\newline))
       (values (apply string collec) i)
       (loop `(,@collec ,char) (add1 i)(peek-char-or-special ip i)))))
(define (reencode ip)
  (define x (get-encode ip))
  (if (string=? x "") ip (reencode-input-port ip  x)))

 (define (filter-unready-port in)
    (let loop ([chars (list)])
      (if (and (char-ready? in)
               (not (eof-object? (peek-char in))))
          (loop (cons (read-char in) chars))
          (open-input-string (apply string (reverse chars))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define build-position-token position-token)
  
 (define (del-/r str)
   (regexp-replace* "\\\r" str ""))
 (define (translate-py-string  raw-str a b)
           (let ((in (open-input-string raw-str))
                 (out (open-output-string)))
             (let loop ()
               (let ((m (regexp-match
                         "\\\\\n|\\\\\\\\|\\\\'|\\\\\"|\\\\0|\\\\a|\\\\b|\\\\f|\\\\n|\\\\r|\\\\t|\\\\v|\\\\[0-3][0-7][0-7]|\\\\x[[:xdigit:]]{2}|\\\\u[[:xdigit:]]{4}|\\\\U[[:xdigit:]]{8}"
         in 0 #f out)))
                 (cond
                   (m (display 
                       (cond
                         ((string=? (~a (car m)) "\\\n") "")
                         ;((string=? (~a (car m)) "\\\r\n")"")
                         ((string=? (~a (car m)) "\\\\") #\\)
                         ((string=? (~a (car m)) "\\'") #\')
                         ((string=? (~a (car m)) "\\\"") #\")
                         ((string=? (~a (car m)) "\\0") #\000)
                         ((string=? (~a (car m)) "\\a") #\007)
                         ((string=? (~a (car m)) "\\b") #\010)
                         ((string=? (~a (car m)) "\\f") #\014)
                         ((string=? (~a (car m)) "\\n") #\012)
                         ((string=? (~a (car m)) "\\r") #\015)
                         ((string=? (~a (car m)) "\\t") #\011)
                         ((string=? (~a (car m)) "\\v") #\013)
                         ((char=? #\x (string-ref (~a (car m)) 1))
                          (integer->char (string->number (substring (~a (car m)) 2 4) 16)))
                         ((char=? #\u (string-ref (~a (car m)) 1))
                          (integer->char (string->number (substring (~a (car m)) 2 6) 16)))
                         ((char=? #\U (string-ref (~a (car m)) 1))
                          (integer->char (string->number (substring (~a (car m)) 2 10) 16)))
                         (else
                          (integer->char (string->number (substring (~a (car m)) 1 4) 8))))
                       out)
                      (loop)))))
             (get-output-string out)))
  ;"^(br|Br|bR|BR|rb|rB|Rb|RB)?(r|R)?('''|'|\"\"\"|\")"
(define (translate-bytes str sp ep)
    ;(write str)(newline)
    (let-values ([(double? single? quote-length)
                  (let ((m (regexp-match "^(br|Br|bR|BR|rb|rB|Rb|RB)?(b|B)?('''|'|\"\"\"|\")" str)))
                    (values (cadr m) (caddr m) (string-length (cadddr m))))])
      (let((bs (del-/r(substring str
                    (+  (if double? 2 1)quote-length)
                      (- (string-length str) quote-length)))))
        (if double? bs
        (translate-py-string bs sp ep)))))

  (define (translate-string str sp ep)
   ; (write str)(newline)
    (let-values (((unicode? raw? quote-length)
                  (let ((m (regexp-match "^(u|U)?(r|R)?('''|'|\"\"\"|\")" str)))
                    (values (cadr m) (caddr m) (string-length (cadddr m))))))
      (let ((raw-str (del-/r(substring str
                                (+ (if (or raw? unicode?)1 0) quote-length)
                                (- (string-length str) quote-length)))))
        (if raw? raw-str (translate-py-string raw-str sp ep)))))
  
(module+ test
         
         )