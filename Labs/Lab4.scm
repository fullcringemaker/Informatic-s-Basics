'№1
(define call/cc call-with-current-continuation)

(define per #f)

(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertions)
     (call/cc
      (lambda (f)
        (set! per f))))))
(define-syntax assert
  (syntax-rules()
    ((assert expr)
     (if (eval expr (interaction-environment))
         "DONE"
         (begin
           (display "FAILED: ")
           (per (display 'expr)))))))
;test
(use-assertions)
(define (1/x x)
  (assert (not (zero? x)))
 (/ 1 x))
(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))
(newline)

'№2
(define (save-data data file-path)
  (with-output-to-file file-path
    (lambda ()
      (write data))))
(define (load-data file-path)
  (with-input-from-file file-path
    (lambda ()
      (read))))

(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define s1 "")
      (define s2 "")
      (define (read-loop c)
        (set! s1 s2)
        (set! s2 (read-char port))
        (if (eof-object? s2)
            c
            (if (or (and (eq? s2 #\return) (not (eq? s1 #\newline)))(and (eq? s2 #\newline) (not (eq? s1 #\newline)) (not (eq? s1 #\return))))
                (read-loop (+ c 1))
                (read-loop c))))
      (read-loop 0))))
;test
(count-line "osn_inf_lab_4.rkt")

'№3
(define (trib-without-memo n)
  (cond
    ((= n 0) 0)
    ((= n 1) 0)
    ((= n 2) 1)
    (else (+ (trib-without-memo (- n 1))
             (trib-without-memo (- n 2))
             (trib-without-memo (- n 3))))))
(trib-without-memo 0)
(trib-without-memo 2)
(trib-without-memo 10)
(trib-without-memo 15)
(newline)
(define trib-with-memo
  (let ((memo (list 0 0 1)))
    (lambda (n)
      (if (< n (length memo))
          (list-ref memo n)
          (let ((result (+ (trib-with-memo (- n 1))
                           (trib-with-memo (- n 2))
                           (trib-with-memo (- n 3)))))
            (set! memo (append memo (list result)))
            result)))))
;test
(trib-with-memo 0)
(trib-with-memo 2)
(trib-with-memo 10)
(trib-with-memo 15)
(newline)

'№4
(define-syntax my-if
  (syntax-rules ()
    ((my-if condition ift iff)
     (force (or (and condition
                     (delay ift))
                (delay iff))))))
;test
(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)
(newline)

'№5
(define-syntax my-let
  (syntax-rules ()
    ((_ ((var expr) ...) body ...)
     ((lambda (var ...) body ...) expr ...))))
(define-syntax my-let*
  (syntax-rules ()
    ((_ () body ...)
     ((lambda () body ...)))
    ((_ ((var expr) p ...) body ...)
     ((lambda (var) (my-let* (p ...) body ...)) expr))))
;test
(define (f1 x y)
  (my-let ((y x) (x y))
    (+ (* 100 x) y)))
(display (f1 2 3))
(newline)

(my-let ((x 2)) x)
(my-let ((x 2) (y 3) (z 4)) (+ x y z))
(my-let* ((x 2) (y (+ x 2))) y)
(newline)

'№6
'№А
(define-syntax when
  (syntax-rules ()
    ((when condition action) (and condition action))                         
    ((when condition . actions) (and condition (begin . actions)))))
(define-syntax unless
  (syntax-rules ()
    ((unless condition action) (if (not condition) action))                         
    ((unless condition . actions) (if (not condition) (begin . actions)))))
;test
(define x 2)
(when (> x 0) (display "x > 0")  (newline))
(unless (= x 0) (display "x != 0") (newline))
(newline)

'№Б
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . actions)
     (letrec ((loop (lambda (xs1)
                      (if (not (null? xs1))
                          (let ((x (car xs1)))
                            (begin (begin . actions) (loop (cdr xs1))))))))
       (loop xs)))
    ((for xs as x . actions) (for x in xs . actions))))

;test
(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

'№В
(define-syntax while
  (syntax-rules ()
    ((while condition . actions)
     (letrec ((loop (lambda ()
                      (if (and condition)
                          (begin (begin . actions) (loop))))))
       (loop)))))
;test
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))
(newline)

'№Г
(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until condition)
     (letrec ((loop (lambda ()
                      (begin (begin . actions)
                             (if (not (and condition))
                                 (loop))))))
       (loop)))))
;test
(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
(newline)

'№Д
(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl) (begin (newline)))
    ((cout << endl << expr ...) (begin (newline) (cout << expr ...)))
    ((cout << v) (begin (display v)))
    ((cout << v << expr ...) (begin (display v) (cout << expr ...)))))
;test
(let ((a 2) (b 3))
  (cout << "a + b = " << (+ a b) << endl))
(cout << "a = " << 2 << endl << "b = " << 3 << endl)





