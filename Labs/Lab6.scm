'№1.1
;<fraction> ::= <optional-sign><numerator>/<denominator>
;<optional-sign> ::= + | - | ε
;<numerator> ::= <digits>
;<denominator> ::= <digits>
;<digits> ::= <digit> | <digit><digits>
;<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

(define (check-frac str)
  (define (search l k)
  (and (not (null? l)) 
       (or (equal? (car l) k) 
           (search (cdr l) k))))
    (define (par1 ls n)
      (cond
        ((and (= n 0) (null? ls)) #f)
        ((and (> n 0) (null? ls)) #t)
        ((search (string->list "0123456789") (car ls)) (par1 (cdr ls) (+ n 1)))
        (else #f)))
    (define (par2 ls n)
      (cond 
        ((null? ls) #f)
        ((and (> n 0) (equal? (car ls) #\/)) (par1 (cdr ls) 0))
        ((search (string->list "0123456789") (car ls)) (par2 (cdr ls) (+ n 1)))
        (else #f)))
    (cond
      ((null? (string->list str)) #f)
      ((search (string->list "+-") (car (string->list str))) (par2 (cdr (string->list str)) 0))
      ((search (string->list "0123456789") (car (string->list str))) (par2 (string->list str) 0))
      (else #f)))

;test
(check-frac "110/111") ; #t
(check-frac "-4/3")    ; #t
(check-frac "+5/10")   ; #t
(check-frac "5.0/10")  ; #f
(check-frac "FF/10")   ; #f
(newline)

'№1.2
;<fraction> ::= <optional-sign><numerator>/<denominator>
;<optional-sign> ::= + | - | ε
;<numerator> ::= <digits>
;<denominator> ::= <digits>
;<digits> ::= <digit> | <digit><digits>
;<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

(define (scan-frac str)
  (define (par-num char-list)
  (let loop ((ch char-list)
             (total 0))
    (if (null? ch)
        total
        (loop (cdr ch) (+ (* total 10) (- (char->integer (car ch)) (char->integer #\0)))))))
  (and (check-frac str)
       (let ((n1 '())
             (n2 '())
             (negative? #f))
         (define (loop ls pos)
           (cond
             ((null? ls) #f)
             ((equal? (car ls) #\/) (loop (cdr ls) 1))
             ((and (= pos 0) (or (equal? (car ls) #\+) (equal? (car ls) #\-)))
              (begin (set! negative? (equal? (car ls) #\-))
                     (loop (cdr ls) 0)))
             ((= pos 0)
              (begin (set! n1 (append n1 (list (car ls))))
                     (loop (cdr ls) 0)))
             ((= pos 1)
              (begin (set! n2 (append n2 (list (car ls))))
                     (loop (cdr ls) 1)))))
         (loop (string->list str) 0)
           (if negative?
               (- (/ (par-num n1) (par-num n2)))
               (/ (par-num n1) (par-num n2))))))

;test
(scan-frac "110/111")  ; 110/111
(scan-frac "-4/3")     ; -4/3
(scan-frac "+5/10")    ; 1/2
(scan-frac "5.0/10")   ; #f
(scan-frac "FF/10")    ; #f
(scan-frac "+/2")      ; #f
(newline)

'№1.3
;<fractions> ::= <fraction> <fraction-separator> <fractions> | <fraction>
;<fraction> ::= <optional-sign><numerator>/<denominator>
;<optional-sign> ::= + | - | ε
;<numerator> ::= <digits>
;<denominator> ::= <digits>
;<fraction-separator> ::= <whitespace> | <whitespace><fraction-separator>
;<whitespace> ::= " " | "\t" | "\n" | "\r"
;<digits> ::= <digit> | <digit><digits>
;<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9


(define (scan-many-fracs str)
  (define (help expr)
    (let loop ((ls expr)
               (hap '())
               (total '())
               (k 0))
      (if (null? ls)
          (if (> k 0)
              (and (scan-frac (list->string hap))
                   (append total (list (scan-frac (list->string hap)))))
              (if (null? total)
                  #f
                  total))
          (if (char-whitespace? (car ls))
              (if (> k 0)
                  (and (scan-frac (list->string hap))
                       (loop (cdr ls) '() (append total (list (scan-frac (list->string hap)))) 0))
                  (loop (cdr ls) '() total 0))
              (loop (cdr ls) (append hap (list (car ls))) total (+ k 1))))))
  (help (string->list str)))

;test
(scan-many-fracs
 "\t1/2 1/3\n\n10/8")  ; (1/2 1/3 5/4)
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")  ; #f
(if (scan-many-fracs "1/2")
    (display 'ok)
    (display 'error))              ;ok
(newline)
(if (scan-many-fracs "1/")
    (display 'ok)
    (display 'error))              ;error
(newline)
(scan-many-fracs "")
;#f
 (scan-many-fracs " ")
;#f
(newline)

'№2
;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .

(define key-words '(define if endif end))

(define (is-key-word? word)
  (member word key-words))

(define x
  (lambda args
    (if (null? args)
        '(() ())
        (apply call-with-current-continuation args))))

(define (parse tokens)
  (if (vector? tokens)
      (if (= (vector-length tokens) 0)
          '(() ())
          (call-with-current-continuation
           (lambda (error)
             (set! x error)
             (program tokens 0))))
      (x)))


(define (program tokens counter)
  (let ((term (vector-ref tokens counter)))
    (cond
      ((equal? term 'define)
       (and (>= (vector-length tokens) (+ counter 2))
            (and (not (or (equal? (vector-ref tokens (+ counter 1)) 'end)
                          (equal? (vector-ref tokens (+ counter 1)) 'define)))
                 (cons (articles tokens (+ counter 1))
                       (list (body tokens (+ (count tokens counter 'end #f) 1) 'top))))))
      (else (cons '() (list (body tokens counter 'top)))))))

(define (articles tokens counter)
  (let ((end (countend tokens counter 'define))
        (next-term (vector-ref tokens counter))) 
    (if (is-key-word? next-term)
        (x #f)
        (cons (article tokens counter) (if end
                                          (articles tokens (+ end 1))
                                          '())))))

(define (article tokens counter)
   (let ((term (vector-ref tokens counter)))
     (if (is-key-word? term)
         (x #f)
         (list term (body tokens (+ counter 1) 1)))))

(define (body tokens counter mark)
  (if (> (+ counter 1) (vector-length tokens))
      '()
      (let ((term (vector-ref tokens counter)))
        (cond
          ((equal? term 'if)
           (cons (list term (body tokens (+ counter 1) 2))
                 (body tokens (helpcoun tokens counter 0) mark)))
          ((equal? term 'define)
           (if (equal? mark 'top)
               (cons term (body tokens (+ counter 1) mark))
               (x #f)))
          ((equal? term 'endif)
           (if (= mark 2)
               '()
               (x #f)))
          ((equal? term 'end)
           (if (= mark 1)
               '()
               (x #f)))
          (else (cons term (body tokens (+ counter 1) mark)))))))

(define (count program counter word res)
  (cond
    ((> (+ counter 1) (vector-length program))
     (or res
         (x #f)))
    ((equal? (vector-ref program counter) word) (count program (+ counter 1) word counter))
    (else (count program (+ counter 1) word res))))


(define (countend program counter word)
  (cond
    ((> (+ counter 1) (vector-length program))
     #f)
     ((equal? (vector-ref program counter) word) counter)
     (else (countend program (+ counter 1) word))))

(define (helpcoun tokens counter res)
    (if (> (+ counter 1) (vector-length tokens))
        (x #f)
 (let ((term (vector-ref tokens counter)))
   (cond
     ((equal? term 'if) (helpcoun tokens (+ counter 1)  (+ res 1)))
     ((equal? term 'endif) (help tokens res  counter )) 
     (else (helpcoun tokens (+ counter 1)  res))))))

(define (help tokens res counter)
  (if (> (+ counter 1) (vector-length tokens))
      (x #f)
      (let ((term (vector-ref tokens counter)))
        (cond
          ((equal? term 'endif) (if (= res 1)
                                    (+ counter 1)
                                    (help tokens (- res 1) (+ counter 1))))
          ((and (equal? term 'if) (not (= res 0))) (x #f))
          (else (help tokens res (+ counter 1)))))))

;test
(parse #(define word w1 w2 w3)) ; #f

(parse #(define end end))
;#f
(parse #(define if end))
;#f
(parse #(define if end endif))
;#f

(parse #(+ + +))
;(()(+ + +))

(parse #(1 2 +))
;(() (1 2 +))
(parse #(x dup 0 swap if drop -1 endif))
              ;(() (x dup 0 swap (if (drop -1))))
       (parse #( define -- 1 - end
                        define =0? dup 0 = end
                        define =1? dup 1 = end
                        define factorial
                        =0? if drop 1 exit endif
                        =1? if drop 1 exit endif
                        dup --
                        factorial
                        *
                        end
                        0 factorial
                        1 factorial
                        2 factorial
                        3 factorial
                        4 factorial ))
              ;(((-- (1 -))(=0? (dup 0 =))(=1? (dup 1 =))(factorial (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
              ; (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial))
(parse #(define word w1 w2 w3))
             ; #f
(parse #(0 if 1 if 2 endif 3 endif 4))
              ;(() (0 (if (1 (if (2)) 3)) 4))
(parse #(define =0? dup 0 = end
                        define gcd
                        =0? if drop exit endif
                        swap over mod
                        gcd
                        end
                        90 99 gcd
                        234 8100 gcd))
              ;(((=0? (dup 0 =))(gcd (=0? (if (drop exit)) swap over mod gcd))) (90 99 gcd 234 8100 gcd))))
(parse #(if endif))
(parse #(if define x end endif))

(define x (parse #()))
(display x)
(newline)

(parse #(if define endif))
;#f

(parse #(define x end define end end))
;#f

(parse #(define x end))



;test
(parse #(define word w1 w2 w3)) ; #f

(parse #(define end end))
;#f
(parse #(define if end))
;#f
(parse #(define if end endif))
;#f

(parse #(+ + +))
;(()(+ + +))

(parse #(1 2 +))
;(() (1 2 +))
(parse #(x dup 0 swap if drop -1 endif))
              ;(() (x dup 0 swap (if (drop -1))))
       (parse #( define -- 1 - end
                        define =0? dup 0 = end
                        define =1? dup 1 = end
                        define factorial
                        =0? if drop 1 exit endif
                        =1? if drop 1 exit endif
                        dup --
                        factorial
                        *
                        end
                        0 factorial
                        1 factorial
                        2 factorial
                        3 factorial
                        4 factorial ))
              ;(((-- (1 -))(=0? (dup 0 =))(=1? (dup 1 =))(factorial (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
              ; (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial))
(parse #(define word w1 w2 w3))
             ; #f
(parse #(0 if 1 if 2 endif 3 endif 4))
              ;(() (0 (if (1 (if (2)) 3)) 4))
(parse #(define =0? dup 0 = end
                        define gcd
                        =0? if drop exit endif
                        swap over mod
                        gcd
                        end
                        90 99 gcd
                        234 8100 gcd))
              ;(((=0? (dup 0 =))(gcd (=0? (if (drop exit)) swap over mod gcd))) (90 99 gcd 234 8100 gcd))))
(parse #(if endif))
(parse #(if define x end endif))

(define x (parse #()))
(display x)
(newline)

(parse #(if define endif))
;#f

(parse #(define x end define end end))
;#f

(parse #(define x end))
