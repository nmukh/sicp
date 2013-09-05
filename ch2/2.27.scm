;2.27
(define (deep-reverse lat)
  (cond
    ((null? lat)
      '())
     ((pair? (car lat))
             (append (deep-reverse (cdr lat))(list(reverse (car lat)))))
     (else
      (append(deep-reverse(cdr lat))(list (car lat))))))
      

