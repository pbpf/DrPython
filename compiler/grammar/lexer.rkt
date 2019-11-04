#lang racket
(require parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           syntax/readerr
           "token.rkt"
           "abbrevs.rkt"
           "tools.rkt"
           )

(provide make-lexer err)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (make-lexer)
    (letrec (
             ;; Keep track of indentation levels for INDENT/DEDENT token generation
             (indent-stack '(0))
             
             ;; Keep track of nesting for implicit line joining
             (p-depth 0)
             (sq-depth 0)
             (c-depth 0)
             (reached-eof #f)
             (last-pos #f)
             (OPAREN (lambda () (set! p-depth (add1 p-depth)) '|(|))
             (CPAREN (lambda () (set! p-depth (sub1 p-depth)) '|)|))
             (OBRACKET (lambda () (set! sq-depth (add1 sq-depth)) '|[|))
             (CBRACKET (lambda () (set! sq-depth (sub1 sq-depth)) '|]|))
             (OBRACE (lambda () (set! c-depth (add1 c-depth)) '|{|))
             (CBRACE (lambda () (set! c-depth (sub1 c-depth)) '|}|))
             
             
             ;; true iff the last token returned was NEWLINE
             (line-start #t)

             ;; the number of DEDENT tokens left to return in a multiple DEDENT line start
             (num-dedents 0)

             ;; Read the spaces at the beginning of the line
             ;; lex-whitespace: input-port -> int
             (lex-whitespace
              (lambda (ip)
                (let loop ((num 0)
                           (count 0))
                  (let ((char (peek-char-or-special ip num)))
                    (cond
                      ((eof-object? char)
                       (read-string num ip)
                       0)
                      ((eq? char #\tab)
                       (loop (add1 num) (+ count 8 (- (modulo count 8)))))
                      ((eq? char #\space)
                       (loop (add1 num) (add1 count)))
                      (else
                       (read-string num ip)
                       count))))))

;             ;; Skips the rest of the line
;             ;; skip-line: input-port -> void
;             [skip-line
;              (lambda (port)
;                (let loop ([char (read-char-or-special port)])
;                  (unless (or (eof-object? char)
;                              (eq? char #\newline))
;                    (loop (read-char-or-special port)))))]
             
             
             ;; The actual lexer
             ;; lex-token: input-port -> token
             (lex-token
              (lexer-src-pos

               ;; 2.1.5, 2.1.6
               ((:: nu-line (:* (:: (:* py-blank) (:?  comment) nu-line)))
                (cond
                  ((and (= 0 p-depth) (= 0 sq-depth) (= 0 c-depth))
                   (set! line-start #t)
                   (set! last-pos end-pos)
                   'NEWLINE)
                  (else
                   (return-without-pos (lex-token input-port)))))
               ;; 2.1.3
               (comment (return-without-pos (lex-token input-port)))
               ;; 2.1.4
               ((:: #\\ (:* py-blank) nu-line) (return-without-pos (lex-token input-port)))
               ;; 2.1.8
               ((:+ py-blank) (return-without-pos (lex-token input-port)))
               
               ;;2.3.1
               (special-keyword
                (string->symbol lexeme))
               ;; 2.3
               (identifer
                (token-NAME lexeme))
               
               
               ;; 2.4.1
               (stringliteral (token-STRING (translate-string lexeme start-pos end-pos)))
               ;; 3.0
               (bytesliteral (token-BYTES  (string->bytes/utf-8 (translate-bytes lexeme start-pos end-pos))))
               ;; 2.4.4
               (decimal-integer
                (token-NUMBER (cons 'int (string->number lexeme))))
               (bin-integer
                (token-NUMBER (cons 'int (string->number (substring lexeme 2 (string-length lexeme)) 2))))
               (oct-integer
                (token-NUMBER (cons 'int (string->number (substring lexeme 2 (string-length lexeme)) 8))))
               (hex-integer
                (token-NUMBER (cons 'int (string->number (substring lexeme 2 (string-length lexeme)) 16))))
               ((:: decimal-integer (:or #\l "L"))
                (token-NUMBER 
                 (cons 'long (string->number (substring lexeme 0 (sub1 (string-length lexeme)))))))
               ((:: oct-integer (:or #\l "L"))
                (token-NUMBER 
                 (cons 'long (string->number (substring lexeme 0 (sub1 (string-length lexeme))) 8))))
               ((:: hex-integer (:or #\l "L"))
                (token-NUMBER
                 (cons 'long (string->number (substring lexeme 2 (sub1 (string-length lexeme))) 16))))
               ;; 2.4.5
               (float
                (token-NUMBER (cons 'float (string->number lexeme))))
               
               ;; 2.4.6
               ((:: (:or float int-part) (:or #\j "J"))
                (token-NUMBER (cons 'complex (* +i (string->number (substring lexeme 0 (sub1 (string-length lexeme))))))))
               
               ;; 2.5
               (binary-operator
                (if (string=? "|" lexeme)
                    '\|
                    (string->symbol lexeme)))
               
               ;; 2.6
               ("(" (OPAREN))
               (")" (CPAREN))
               ("[" (OBRACKET))
               ("]" (CBRACKET))
               ("{" (OBRACE))
               ("}" (CBRACE))
               ((:or misc-operator #\\)
                (string->symbol lexeme))
               
               ;; ignore special objects such as images
               [(special) (return-without-pos (lex-token input-port))]

               [(eof)
                (cond
                  (reached-eof 'EOF)
                  (else
                   (set! reached-eof #t)
                   (set! line-start #t)
                   (set! last-pos end-pos)
                   'NEWLINE))]
               
               [any-char
                (raise-read-error
                 (format "invalid character '~a',line ~a col ~a" lexeme (position-line start-pos)(position-col start-pos))
                 (file-path)
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos) (position-offset start-pos)))]))
                 
             ;; Return the number of entries in l before n is reached and set the stack to
             ;; be the rest of l (after n)
             ;;count-dedent: int * int list -> int
             (count-dedent
              (lambda (n l)
                (cond
                  ((null? l) #f)
                  ((= n (car l)) 
                   (set! indent-stack l)
                   0)
                  ((< (car l) n)
                   (raise-read-error
                    "unmatching indentation"
                    (file-path)
                    (and last-pos (position-line last-pos))
                    (and last-pos (position-col last-pos))
                    (and last-pos (position-offset last-pos))
                    n))
                  (else
                   (add1 (count-dedent n (cdr l)))))))
             ;; get-token: input-port -> token
             (get-token
              (lambda (ip)
                (cond
                  ;; There are still DEDENTS to do
                  ((> num-dedents 0) 
                   (set! num-dedents (sub1 num-dedents))
;                   (printf "PARSER: there are still DEDENTS to do~n")
                   (build-position-token 'DEDENT last-pos last-pos))
                  ;; We are at the start of a line
                  (line-start
                   (let ((indent-num (lex-whitespace ip)))
                     (cond
                       ;; Same indentation level as last line, find the first token on the line
                       ((= indent-num (car indent-stack))
                        (set! line-start #f)
;                        (printf "PARSER: same indentation level as last line~n")
                        (get-token ip))
                       ;; indented from last line
                       ((> indent-num (car indent-stack))
                        (set! line-start #f)
                        (set! indent-stack (cons indent-num indent-stack))
;                        (printf "PARSER: indented from last line~n")
                        (build-position-token 'INDENT last-pos last-pos))
                       (else
                        (set! line-start #f)
;                        (printf "PARSER: else~n")
                        (set! num-dedents (sub1 (count-dedent indent-num indent-stack)))
                        (build-position-token 'DEDENT last-pos last-pos)))))
                  (else
                   (lex-token ip))))))
      get-token))
  
  
  (define (err token-ok token-name token-value src-start src-end)
    (raise-read-error
     (if token-ok
         (format "invalid or incomplete syntax line ~a,row ~a,offset ~a ,token ~a (value: ~s)"(position-line src-start) (position-col src-start) (position-offset src-start)token-name token-value)
         (format "unrecognized line ~a,row ~a,offset ~a token ~a (value: ~a)" (position-line src-start) (position-col src-start) (position-offset src-start) token-name token-value))
     (file-path)
     (position-line src-start)
     (position-col src-start)
     (position-offset src-start)
     (- (position-offset src-end) (position-offset src-start))))

  (define (is-subscription? x)
    (and (not (list? x)) (not (eq? '... x))))

  (define (is-simple-slice? x)
    (and (list? x) (= 2 (length x))))

(module+ test
  
  (provide collect-tokens complete-statement? ready-to-submit?)
  
    (define (collect-tokens p1)
      (define port (reencode p1))
       (port-count-lines! port)
      
    (let ([lexer (make-lexer)])
      (let loop ([collected (list)])
        (let ([token (lexer port)])
          ;(printf "~a\n" token)
          (if (eq? (position-token-token token) 'EOF)
              (reverse collected)
              (loop (cons token collected)))))))
  
  (define (complete-statement? port)
    (let* ([tokens (collect-tokens port)]
           [tokens (for/list ([tok tokens]
                              #:break (and (eq? (position-token-token tok) 'NEWLINE)
                                           (= (position-offset (position-token-start-pos tok))
                                              (position-offset (position-token-end-pos tok)))))
                     tok)])
      (cond
        [(empty? tokens) #t]
        [(eq? (position-token-token (last tokens)) ':) #f]
        [(eq? (position-token-token (last tokens)) '|\|) #f]
        [(let ([indents (for/sum ([tok tokens])
                          (if (eq? (position-token-token tok) 'INDENT) 1 0))]
               [dedents (for/sum ([tok tokens])
                          (if (eq? (position-token-token tok) 'DEDENT) 1 0))])
           (< dedents indents)) #f]
        [else #t])))
         
  (define (ready-to-submit? port whitespace?)
    (and whitespace?
         (complete-statement? (filter-unready-port port))))
  
  ;(define x (open-input-string "\"Österreich\""))
  ;"D:\\SoftWares\\math and pro\\Python-3.4.2\\Python-3.4.2\\Python-3.4.2\\Lib\\sqlite3\\test\\factory.py"
 ; (collect-tokens x)
 ;(collect-tokens (open-input-file "D:\\SoftWares\\math and pro\\Python-3.4.2\\Python-3.4.2\\Python-3.4.2\\Lib\\sqlite3\\test\\factory.py"))
  )
  