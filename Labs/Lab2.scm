;;№1
(define (count x xs)
  (if (null? xs)
      0
      (if (equal? (car xs) x)
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))

;;№2
(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))))

;;№3
(define (iterate f x n)
  (if (= n 0)
      '()
      (cons x (iterate f (f x) (- n 1)))))

;;№4
(define (intersperse e xs)
  (if (< (length xs) 2)
      xs
      (cons (car xs) (cons e (intersperse e (cdr xs))))))

;;№5
(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (any? pred? (cdr xs)))))
(define (all? pred? xs)
  (if (null? xs)
      #t
      (and (pred? (car xs)) (all? pred? (cdr xs)))))

;;№6
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
