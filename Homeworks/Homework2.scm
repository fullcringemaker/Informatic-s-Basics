'№1
;1.1
(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))))
(my-range  0 11 3)
(newline)

;1.21
(define (my-flatten lst)
  (define (flatten-helper lst acc)
    (if (null? lst)
          acc
          (if (pair? (car lst))
              (flatten-helper (cdr lst) (flatten-helper (car lst) acc))
              (flatten-helper (cdr lst) (cons (car lst) acc)))))
  (reverse (flatten-helper lst '())))

;1.2 (ачивка)
(define (my-flatten l)
  (define (loop res l)
    (if (null? l)
        res
        (if (list? l)
            (loop (loop res (cdr l)) (car l))
            (cons l res))))
  (loop '() l))
(my-flatten '(1 2 (3 4 (5 6)) 7))
(my-flatten '((1) 2 (3 (4 5)) 6))
(newline)

;1.3
(define (my-element? elem xs)
  (and (not (null? xs)) (or (equal? elem (car xs)) (my-element? elem (cdr xs)))))
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))
(newline)

;1.4
(define (my-filter pred? lst)
  (if (null? lst)
      '()
      (if (pred? (car lst))
          (cons (car lst) (my-filter pred? (cdr lst)))
          (my-filter pred? (cdr lst)))))
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
(newline)

;1.5
(define (my-fold-left op xs)
  (if (<= (length xs) 1)
      xs
      (my-fold-left op (cons (op (car xs) (cadr xs)) (cddr xs)))))
(my-fold-left  quotient '(16 2 2 2 2)) 
(my-fold-left  quotient '(1))          
(newline)

;1.6
(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

(my-fold-right expt '(2 3 4))
(my-fold-right expt '(2))
(newline)

'№2
;2.1
(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (append (list->set (cdr xs))(list (car xs))))))
(list->set '(1 1 2 3)) 
(newline)

;2.2
(define (set? xs)
  (if (null? xs)
      #t
      (if (my-element? (car xs) (cdr xs))
          #f
          (set? (cdr xs)))))
(set? '(1 2 3))   
(set? '(1 2 3 3))                            
(set? '())
(newline)

;2.3
(define (union xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (if (my-element? (car xs) ys)
              (union (cdr xs) ys)
              (union (cdr xs) (cons (car xs) ys))))))
(union '(1 2 3) '(2 3 4))
(newline)

;2.4
(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (null? ys)
          '()
          (if (my-element? (car xs) ys)
              (cons (car xs) (intersection (cdr xs) ys))
              (intersection (cdr xs) ys)))))
(intersection '(1 2 3) '(2 3 4))
(intersection '() '(1 2 3))
(intersection '(1 2 3) '())
(newline)   

;2.5
(define (difference xs ys)
  (if (null? xs)
      '()
      (if (null? ys)
          xs
          (if (my-element? (car xs) ys)
              (difference (cdr xs) ys)
              (cons (car xs) (difference (cdr xs) ys))))))
(difference '(1 2 3 4 5) '(2 3))
(difference '() '(1 2 3))
(difference '(1 2 3 4) '())
(newline)

;2.6
(define (symmetric-difference xs ys)
  (union (reverse (difference xs ys)) (difference ys xs)))
(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(newline)

;2.7
(define (set-eq? xs ys)
  (equal? (difference (union xs ys) (intersection xs ys)) '()))
(set-eq? '(1 2 3) '(3 2 1))                 
(set-eq? '(1 2) '(1 3))  
(newline)

'№3
;3.1
(define (string-trim-left s)
  (if (null? (string->list s))
      ""
      (if (char-whitespace? (string-ref s 0))
          (string-trim-left (substring s 1))
          s)))
(string-trim-left "\t\tabc def")
(newline)

(define (str-reverse s)
  (list->string (reverse (string->list s))))
(define (string-trim-right s)
  (if (null? (string->list s))
      ""
      (str-reverse (string-trim-left (str-reverse s)))))
(string-trim-right "abc def\t")
(newline)

(define (string-trim s)
  (if (null? (string->list s))
      ""
      (string-trim-right (string-trim-left s))))
(string-trim       "\t abc def \n")
(newline)

;3.2
(define (string-prefix? a b)
  (if (null? (string->list a))
      #t
      (if (null? (string->list b))
          #f
          (if (equal? (car (string->list a)) (car (string->list b)))
              (string-prefix? (list->string (cdr (string->list a))) (list->string (cdr (string->list b))))
              #f))))
(string-prefix? "abc" "abcdef") 
(string-prefix? "bcd" "abcdef") 
(string-prefix? "abcdef" "abc")
(newline)

(define (string-suffix? a b)
  (if (null? (string->list a))
      #t
      (if (null? (string->list b))
          #f
          (if (equal? (car (reverse (string->list a))) (car (reverse (string->list b))))
              (string-suffix? (list->string (reverse (cdr (reverse (string->list a))))) (list->string (reverse (cdr (reverse (string->list b))))))
              #f))))
(string-suffix? "def" "abcdef")
(string-suffix? "bcd" "abcdef")
(newline)

(define (string-infix? a b)
  (if (null? (string->list a))
      #t
      (if (null? (string->list b))
          #f
           (or (string-suffix? a b) (string-prefix? a b)
              (string-infix? a (list->string (cdr (string->list b))))
              (string-infix? a (list->string (cdr (string->list b))))))))
(string-infix? "def" "abcdefgh") 
(string-infix? "abc" "abcdefgh") 
(string-infix? "fgh" "abcdefgh") 
(string-infix? "ijk" "abcdefgh") 
(string-infix? "bcd" "abc")
(newline)

;3.3
(define (func str sep)
  (if (or (string-prefix? sep str) (= (string-length str) 0))
      ""
      (string-append (make-string 1 (string-ref str 0)) (func (substring str 1) sep))))
(define (string-split str sep)
  (if (null? (string->list str))
      '()
      (if (string-prefix? sep str)
          (string-split (substring str (string-length sep)) sep)
          (cons (func str sep) (string-split (substring str (string-length (func str sep))) sep)))))
(string-split "x;y;z" ";")
(string-split "x-->y-->z" "-->")
(string-split "abc;def;ghi" ";")
(newline)

'№4
;4.1
(define (funct n)
  (if (null? n)
      1
      (* (car n) (funct (cdr n)))))
(define (make-multi-vector sizes . fill)
  (define (incl r . g)
    (if (not (null? fill))
        (make-vector (funct sizes) (car fill))
        (make-vector (funct sizes) 0)))
  (list sizes (incl sizes fill)))
(define m (make-multi-vector '(11 12 9 16)))
(newline)

;4.2
(define (multi-vector? m)
  (and (list? m)(not (null? (car m))) (eq? (funct (car m)) (vector-length (cadr m)))))
(multi-vector? m)
(newline)

;4.3
(define (helfun x y)
  (if (equal? y '())
      0
      (+ (* (car y) (funct (cdr x))) (helfun (cdr x) (cdr y)))))
(define (multi-vector-set! m indices x)
  (vector-set! (cadr m) (helfun (car m) indices) x))
(multi-vector-set! m '(10 7 6 12) 'test)
(newline)

;4.4
(define (multi-vector-ref m indices)
  (vector-ref (cadr m) (helfun (car m) indices)))
(multi-vector-ref m '(10 7 6 12))
(newline)

'№5
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))
(define (o . xs)
  (lambda (x)
    (define (podpr xs x)
      (if (null? xs)
          x
          (podpr (cdr xs) ((car xs) x))))
    (podpr (reverse xs) x)))
((o f g h) 1)
((o f g) 1)   
((o h) 1)     
((o) 1)
