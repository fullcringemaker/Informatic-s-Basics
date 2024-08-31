;;N1
(define (day-of-week day month year)
  (define (counter day month year)
    (if (< month 3)
        (counter day (+ month 12) (- year 1))
        (let* ((B (remainder year 100))
               (A (quotient year 100)))
          (remainder (+ day (quotient (* 13 (+ month 1)) 5) B (quotient B 4) (quotient A 4) (* -2 A)) 7))))

  (define (normalize-day-of-week d)
    (if (= d 0)
        6
        (- d 1)))

  (normalize-day-of-week (counter day month year)))

;;N2
(define (solve a b c)
  (if (and (= a 0) (= b 0))
      (list)
      (if (and (= a 0) (not (= b 0)))
          (list (/ (* -1 c) b))
            (if (and (> (- (* b b) (* 4 a c)) 0) (not (= a 0)))
                (list (/ (+ (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)) (/ (- (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
                (if (and (= (- (* b b) (* 4 a c)) 0) (not (= a 0)))
                    (list (/ (+ (* -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
                    (if (and (< (- (* b b) (* 4 a c)) 0) (not (= a 0)))
                        (list)))))))

;;№3
(define (my-gcd a b)
  (if (or (= a 0) (= b 0))
      (max a b)
      (my-gcd b (remainder a b))))

(define (my-lcm a b)
  (if (or (= a 0) (= b 0))
      0
      (quotient (* a b) (my-gcd a b))))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(define (prime? n)
  (if (= (remainder (fact (- n 1)) n) (- n 1))
      #t
      #f))
          
