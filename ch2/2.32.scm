;2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x)
                           (car x)) 
                            rest)))))