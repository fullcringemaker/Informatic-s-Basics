'№1
(define memoized-factorial
  (let ((known-results '()))
    (lambda (x)
      (if (>= 1 x)
          1
          (let ((total (assoc x known-results)))
            (if total
                (cadr total)
                (let ((total (* (memoized-factorial (- x 1)) x)))
                  (set! known-results (cons (list x total) known-results))
                  total)))))))

;test
(memoized-factorial 10)
(newline)
(memoized-factorial 20)
(newline)
(memoized-factorial 50)
(newline)
