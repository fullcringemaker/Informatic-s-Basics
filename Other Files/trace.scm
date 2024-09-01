(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex expr)
     (begin
       (display `expr)
       (display " => ")
       (let ((res expr))
         (display res)
         (newline)
         res)))))
