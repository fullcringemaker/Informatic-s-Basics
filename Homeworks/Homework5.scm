(define feature-if-else #t)
(define feature-nested-if #t)
(define feature-while-loop #t)
(define feature-repeat-loop #t)
(define feature-break-continue #t)
(define feature-switch-case #t)


(define ie (interaction-environment))

(define math '(+ - *))
(define compare '(= > <))
(define logic '(and or))

(define (in? xs x)
  (and (not (null? xs)) (or (equal? (car xs) x) (in? (cdr xs) x))))

(define (math-act operation stack)
  (cons (run-operation operation stack) (cddr stack)))

(define (compare-act operation stack)
  (cons (if (run-operation operation stack) -1 0) (cddr stack)))

(define (run-operation operation stack)
  (eval (list operation (cadr stack) (car stack)) ie))

(define (word-ind word program ind) 
  (if (< ind (vector-length program))
      (if (equal? (vector-ref program ind) word)
          ind
          (word-ind word program (+ ind 1)))
      #f))

(define (jump-if program index)
  (let loop ((cnt-if 1) (ind index))
    (if (zero? cnt-if)
        ind
        (let ((word (vector-ref program ind)))
          (if (equal? word 'if)
            (loop (+ cnt-if 1) (+ 1 ind))
            (if (and (= cnt-if 1) (equal? word 'else))
                (+ 1 ind)
                (if (equal? word 'endif)
                    (loop (- cnt-if 1) (+ 1 ind))
                    (loop cnt-if (+ 1 ind)))))))))

(define (jump-while program index)
  (let loop ((cnt-while 1) (ind index))
    (if (zero? cnt-while)
        ind
        (let ((word (vector-ref program ind)))
          (if (equal? word 'while)
            (loop (+ cnt-while 1) (+ 1 ind))
            (if (equal? word 'wend)
                (loop (- cnt-while 1) (+ 1 ind))
                (loop cnt-while (+ 1 ind))))))))

(define (first-exit program ind)
  (let ((word (vector-ref program ind)))
    (cond
      ((or (equal? word 'until) (equal? word 'wend)) ind)
      (else (first-exit program (+ ind 1))))))

(define (case-const num program index)
  (let loop ((ind (word-ind 'case program index)))
    (if ind
        (if (equal? num (vector-ref program (+ ind 1)))
            (+ ind 2)
            (loop (word-ind 'case program (+ ind 1))))
        (word-ind 'endswitch program index))))
    
        
(define (interpret program init-stack)
  (let loop ((ind 0) (stack init-stack) (return-stack '()) (dict '()))
    (if (= (vector-length program) ind)
        stack
        (let ((word (vector-ref program ind)) #|(x (begin (display stack) (newline)))|#)
          (cond
            ((number? word) (loop (+ ind 1) (cons word stack) return-stack dict))
            ((in? math word) (loop (+ ind 1) (math-act word stack) return-stack dict))
            ((equal? '/ word) (loop (+ ind 1) (cons (quotient (cadr stack) (car stack)) (cddr stack)) return-stack dict))
            ((equal? 'mod word) (loop (+ ind 1) (cons (remainder (cadr stack) (car stack)) (cddr stack)) return-stack dict))
            ((equal? 'neg word) (loop (+ ind 1) (cons (- (car stack)) (cdr stack)) return-stack dict))
            ((in? compare word) (loop (+ ind 1) (compare-act word stack) return-stack dict))
            ((equal? 'not word) (loop (+ ind 1) (cons (if (= (car stack) 0) -1 0) (cdr stack)) return-stack dict))
            ((equal? 'and word) (loop (+ ind 1) (cons (if (and (not (= (car stack) 0)) (not (= (cadr stack) 0))) -1 0) (cddr stack)) return-stack dict))
            ((equal? 'or word) (loop (+ ind 1) (cons (if (and (= (car stack) 0) (= (cadr stack) 0)) 0 -1) (cddr stack)) return-stack dict))
            ((equal? 'drop word) (loop (+ ind 1) (cdr stack) return-stack dict))
            ((equal? 'swap word) (loop (+ ind 1) (append (list (cadr stack) (car stack)) (cddr stack)) return-stack dict))
            ((equal? 'dup word) (loop (+ ind 1) (cons (car stack) stack) return-stack dict))
            ((equal? 'over word) (loop (+ ind 1) (cons (cadr stack) stack) return-stack dict))
            ((equal? 'rot word) (loop (+ ind 1) (append (list (caddr stack) (cadr stack) (car stack)) (cdddr stack)) return-stack dict))
            ((equal? 'depth word) (loop (+ ind 1) (cons (length stack) stack) return-stack dict))
            ((equal? 'define word) (loop (+ 1 (word-ind 'end program ind)) stack return-stack (cons (list (vector-ref program (+ ind 1)) (+ ind 2)) dict)))
            ((in? '(exit end) word) (loop (car return-stack) stack (cdr return-stack) dict))
            ((equal? 'if word) (if (zero? (car stack))
                                        (loop (jump-if program (+ ind 1)) (cdr stack) return-stack dict)
                                        (loop (+ ind 1) (cdr stack) return-stack dict)))
            ((equal? 'else word) (if (zero? (car stack))
                                     (loop (+ ind 1) (cdr stack) return-stack dict)
                                     (loop (+ 1 (word-ind 'endif program ind)) stack return-stack dict)))
            ((equal? 'endif word) (loop (+ ind 1) stack return-stack dict))
            ((equal? 'while word) (if (zero? (car stack))
                                      (loop (jump-while program (+ ind 1)) (cdr stack) return-stack dict)
                                      (loop (+ ind 1) (cdr stack) (cons ind return-stack) dict)))
            ((equal? 'wend word) (loop (car return-stack) stack (cdr return-stack) dict))
            ((equal? 'repeat word) (loop (+ ind 1) stack (cons ind return-stack) dict))
            ((equal? 'until word) (loop (if (zero? (car stack)) (car return-stack) (+ 1 ind)) (cdr stack) (cdr return-stack) dict))
            ((equal? 'break word) (loop (+ 1 (first-exit program ind)) stack (cdr return-stack) dict))
            ((equal? 'continue word) (loop (first-exit program ind) stack return-stack dict))
            ((equal? 'switch word) (loop (case-const (car stack) program ind) (cdr stack) return-stack dict))
            ((equal? 'case word) (loop (+ ind 2) stack return-stack dict))
            ((equal? 'exitcase word) (loop (word-ind 'endswitch program ind) stack return-stack dict))
            ((equal? 'endswitch word) (loop (+ ind 1) stack return-stack dict))
            (else (loop (cadr (assoc word dict)) stack (cons (+ ind 1) return-stack) dict)))))))

(interpret #(   define abs
                          dup 0 <
                          if neg endif
                          end
                          9 abs
                          -9 abs      ) (quote ())) ;(9 9)
(interpret #(   define =0? dup 0 = end
                          define <0? dup 0 < end
                          define signum
                          =0? if exit endif
                          <0? if drop -1 exit endif
                          drop
                          1
                          end
                          0 signum
                          -5 signum
                          10 signum       ) (quote ())) ;(1 -1 0)
