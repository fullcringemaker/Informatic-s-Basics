;№1
'№1
(load "trace.scm")
(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss)))) 
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))
(zip '(+ 1 3) '(plus one three))
(newline)

;№2
'№2
(load "unit-test.scm")
(define (counter x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) 
    (else     1)))
(define the-tests
  (list (test (counter -2) -1)
        (test (counter  0)  0)
        (test (counter  2)  1)))
(define (counter x)
  (cond
    ((< x 0) -1)
    ((= x 0)  0) 
    (else     4)))
(define counter-tests
  (list (test (counter 3) 4)
        (test (counter 7) 7)
        (test (counter 5) 4)))
(run-tests counter-tests)


(load "trace.scm")
(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ 1 n))
      n)))
(+ (trace-ex (counter)) (trace-ex (counter)))

(run-tests the-tests)
(newline)

;№3
'№3
(define (ref xs . args)
  (define (get-elem i xs1 arg)
    (if (null? xs1)
        #f
        (if (= i arg)
            (car xs1)
            (get-elem (+ i 1) (cdr xs1) arg))))

  (define (insert-elem i xs1 pos elem)
    (if (null? xs1)
        (if (= pos i)
            (cons elem '())
            '())
        (if (= pos i)
            (cons elem xs1)
            (cons (car xs1) (insert-elem (+ i 1) (cdr xs1) pos elem)))))

  (define s
    (cond ((vector? xs) (vector->list xs))
          ((string? xs) (string->list xs))
          (else xs)))

  (if (= (length args) 1)
      (get-elem 0 s (car args))
      (cond ((vector? xs)
             (and (<= 0 (car args)) (<= (car args) (vector-length xs))
                  (list->vector (insert-elem 0 s (car args) (cadr args)))))
            ((string? xs)
             (and (<= 0 (car args)) (<= (car args) (string-length xs)) (char? (cadr args))
                  (list->string (insert-elem 0 s (car args) (cadr args)))))
            (else
             (and (<= 0 (car args)) (<= (car args) (length xs))
                  (insert-elem 0 s (car args) (cadr args)))))))
(ref '(1 2 3) 1) 
(ref #(1 2 3) 1) 
(ref "123" 1)    
(ref "123" 3)    
(ref '(1 2 3) 1 0)   
(ref #(1 2 3) 1 0)  
(ref #(1 2 3) 1 #\0) 
(ref "123" 1 #\0)    
(ref "123" 1 0)     
(ref "123" 3 #\4)   
(ref "123" 5 #\4)   
(newline)

;№4
'№4
(define (factorize expr)
  (cond ((2-? expr) (factorize-2- expr))
        ((3-? expr) (factorize-3- expr))
        ((3+? expr) (factorize-3+ expr))
        (else (display "error"))))

(define (2-? expr)
  (and (list? expr)
       (equal? '- (car expr))
       (expt-? (cadr expr) 2)))

(define (factorize-2- expr)
  (let* ((a (base expr))
         (b (exponent expr)))
    `(* (- ,a ,b)
        (+ ,a ,b))))

(define (3-? expr)
  (and (list? expr)
       (equal? '- (car expr))
       (expt-? (cadr expr) 3)))

(define (factorize-3- expr)
  (let* ((a (base expr))
         (b (exponent expr)))
    `(* (- ,a ,b)
        (+ (* ,a ,a) (* ,a ,b) (* ,b ,b)))))

(define (3+? expr)
  (and (list? expr)
       (equal? '+ (car expr))
       (expt-? (cadr expr) 3)))

(define (factorize-3+ expr)
  (let* ((a (base expr))
         (b (exponent expr)))
    `(* (+ ,a ,b)
        (+ (- (* ,a ,a) (* ,a ,b)) (* ,b ,b)))))

(define (expt-? expr n)
  (and (list? expr)
       (equal? 'expt (car expr))
       (equal? n (caddr expr))))

(define (base expr)
  (cadr (cadr expr)))

(define (exponent expr)
  (caddr (cadr expr)))
(factorize '(- (expt x 2) (expt y 2))) 
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
(eval (list (list 'lambda 
                  '(x y) 
                  (factorize '(- (expt x 2) (expt y 2))))
            1 2)
      (interaction-environment))
(newline)



(define (counter x)
  (cond
    ((< x 0) -1)
    ((= x 0)  0) 
    (else     4)))
(define counter-tests
  (list (test (counter 3) 4)
        (test (counter 7) 7)
        (test (counter 5) 4)))
(run-tests counter-tests)


(load "trace.scm")
(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ 1 n))
      n)))
(+ (trace-ex (counter)) (trace-ex (counter)))










