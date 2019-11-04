#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (all-defined-out))

(define-lex-abbrevs
   (nu-line (:or (:: #\return #\newline) #\newline))
   (py-blank (:or #\space #\tab #\page))
   (comment (:: #\# (:* (:~ #\return #\newline))))
  ;---------
   (letter (:or (:/ #\A #\Z) (:/ #\a #\z)))
   (digit (:/ #\0 #\9))
  ;---------------------------------
   (decimal-integer (:or (:+ "0") (:: (:/ "1" "9") (:* digit))))
   (oct-integer (:: "0" (:or #\o "O")(:+ (:/ "0" "7"))))
   (bin-integer (:: "0" (:or #\b "B")(:+ (:/ "0" "1"))))
   (hex-integer (:: "0" (:or #\x "X") (:+ (:or digit (:/ "A" "F") (:/ "a" "f")))))
  ;------------------------------------
   (float (:or point-float exponent-float))
   (point-float (:or (:: (:?  int-part) fraction)
                   (:: int-part ".")))
   (exponent-float (:: (:or int-part point-float) exponent))
   (int-part (:+ digit))
   (fraction (:: "." (:+ digit)))
   (exponent (:: (:or #\e "E") (:?  (:or #\+ #\-)) (:+ digit)))
  ;----------------------------------------------------------------
   (special-keyword (:or 	"False" 	"class" 	"finally" 	"is" 		"return" 	"None" 
				"continue" 	"for" 		"lambda" 	"try" 		"True" 		"def" 
				"from" 		"nonlocal" 	"while" 	"and"		"del"   	"global"
				"not" 		"with" 		"as" 		"elif" 		"if" 		"or" 
				"yield" 	"assert" 	"else" 		"import" 	"pass" 		"break" 
				"except" 	"in" 		"raise"
                         ))
  (reserved-classes-of-identifiers (:or "-*" "--*--"  "--*"))
  ;--------------------
   (binary-operator (:or #\+       #\-       "**"  #\*    #\/       "//"      #\%
                         "<<"      ">>"      #\&       #\|       #\^       #\~
                         #\<       #\>       "<="      ">="      "=="      "!="      "<>"))
   (misc-operator (:or #\, #\; #\. #\` #\@ "..." ":="
                       #\:       #\=       "+="      "-="      "*="      "/="      "//="     "%="  "->"
                       "|="      "&="      "^="      ">>="     "<<="     "**=")))
(define-lex-abbrevs
   (identifer (:: id_start (:* id_continue)))
   (id_start  (:or alphabetic #\_))
   (id_continue (:or id_start digit))
   (stringliteral (:: (:? string-prefix) (:or short-string1
                                         short-string2
                                         long-string1
                                         long-string2)))
   (string-prefix (:or #\u #\U  #\r #\R))
   (short-string1 (:: "'" (:* short-string-item1)"'"))
   (short-string2 (:: #\" (:* short-string-item2) #\"))
   (long-string1 (:: "'''" (:* (:or long-string-item1
                                (:: #\' long-string-item1)
                                (:: "''" long-string-item1)))
                  "'''"))
   (long-string2 (:: "\"\"\"" (:* (:or long-string-item2
                                (:: #\" long-string-item2)
                                (:: "\"\"" long-string-item2)))
                  "\"\"\""))
   (short-string-item1* (:or short-string-char1 escape-seq))
  (short-string-item1(:or short-string-item1*
                          (::  #\\ #\return #\newline)))
   (short-string-item2* (:or short-string-char2 escape-seq))
  (short-string-item2(:or short-string-item2*
                          (::  #\\ #\return #\newline)))
   (long-string-item1* (:or long-string-char1 escape-seq))
  (long-string-item1(:or long-string-item1*
                          (::  #\\ #\return #\newline)))
   (long-string-item2* (:or long-string-char2 escape-seq))
   (long-string-item2(:or long-string-item2*
                          (::  #\\ #\return #\newline)))
   (short-string-char1 (:~ #\\ #\return #\newline #\'))
   (short-string-char2 (:~ #\\ #\return #\newline #\"))
   (long-string-char1 (:~ #\\ #\'))
   (long-string-char2 (:~ #\\ #\"))
   (escape-seq (:: #\\ any-char))
  ;---------------
  #| (string-literal (:: (:? stringprefix) (:or shortstring longstring)))
   (stringprefix  (:or #\u #\U  #\r #\R))
   (shortstring (:or (:: #\' (:* shortstringitem1) #\')
                     (:: #\" (:* shortstringitem2) #\")))
   (longstring  (:or (:: "'''" (:*  longstringitem) "'''")(:: "\"\"\"" (:*  longstringitem) "\"\"\"")))
   (shortstringitem1 (:or  shortstringchar1   stringescapeseq))
   (shortstringitem2 (:or  shortstringchar2   stringescapeseq))
   (longstringitem  (:or  longstringchar   stringescapeseq))
   (shortstringchar1 (:~ #\\ #\return #\newline #\'))
   (shortstringchar2 (:~ #\\ #\return #\newline #\"))
   (longstringchar (:~ #\\))
   (stringescapeseq (:: #\\ (:/ #\000 #\377)))|#
  ;------------------------------------------
  (bytesliteral (::  bytes-prefix (:or short-bytes1
                                         short-bytes2
                                         long-bytes1
                                         long-bytes2)))
   (bytes-prefix (:or   "b" "B" "br"   "Br"  "bR"   "BR"   "rb"   "rB"   "Rb"   "RB"))
   (short-bytes1 (:: "'" (:* short-bytes-item1) "'"))
   (short-bytes2 (:: #\" (:* short-bytes-item2) #\"))
   (long-bytes1 (:: "'''" (:* (:or long-bytes-item1
                                (:: #\' long-bytes-item1)
                                (:: "''" long-bytes-item1)))
                  "'''"))
   (long-bytes2 (:: "\"\"\"" (:* (:or long-bytes-item2
                                (:: #\" long-bytes-item2)
                                (:: "\"\"" long-bytes-item2)))
                  "\"\"\""))
   (short-bytes-item1* (:or short-bytes-char1 bytes-escape-seq))
  (short-bytes-item1(:or short-bytes-item1*
                          (::  #\\ #\return #\newline)))
   (short-bytes-item2* (:or short-bytes-char2 bytes-escape-seq))
  (short-bytes-item2(:or short-bytes-item2*
                          (::  #\\ #\return #\newline)))
   (long-bytes-item1* (:or long-bytes-char1 bytes-escape-seq))
  (long-bytes-item1(:or long-bytes-item1*
                          (::  #\\ #\return #\newline)))
   (long-bytes-item2* (:or long-bytes-char2 bytes-escape-seq))
   (long-bytes-item2(:or long-bytes-item2*
                          (::  #\\ #\return #\newline)))
   (short-bytes-char1 (:~ #\\ #\return #\newline #\'))
   (short-bytes-char2 (:~ #\\ #\return #\newline #\"))
   (long-bytes-char1 (:~ #\\ #\'))
   (long-bytes-char2 (:~ #\\ #\"))
   (bytes-escape-seq (:: #\\ (:/ #\000 #\255)))
  ;----------------------------------
  #|
  (bytes-literal   (:: bytesprefix(:or shortbytes  longbytes)))
  (bytesprefix    (:or   "b" "B" "br"   "Br"  "bR"   "BR"   "rb"   "rB"   "Rb"   "RB"))
  (shortbytes     (:or (::  "'" (:* shortbytesitem1) "'") (:: #\" (:* shortbytesitem2)  #\")))
  (longbytes      (:or (:: "'''" (:* longbytesitem) "'''")
                       (:: "\"\"\"" (:* longbytesitem) "\"\"\"")))
  (shortbytesitem1 (:or shortbyteschar1 bytesescapeseq))
  (shortbytesitem2 (:or shortbyteschar2 bytesescapeseq))
  (longbytesitem  (:or longbyteschar   bytesescapeseq))
  (shortbyteschar1 (:~ #\\ #\return #\newline #\'))
  (shortbyteschar2 (:~ #\\ #\return #\newline #\"))
  (longbyteschar  (:~ #\\))
  (bytesescapeseq (:: #\\ (:/ #\000 #\255)))|#
  )