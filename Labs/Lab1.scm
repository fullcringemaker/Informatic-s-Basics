;;№1.1
(define (my-even x)
  (if (= (remainder x 2) 0)
      #t
      #f))
;;№1.2
(define (my-odd x)
  (if (= (remainder x 2) 1)
      #t
      #f))

;;№2
(define (power base exp)
  (if (= exp 1)
      base
      (* base (power base (- exp 1)))))
