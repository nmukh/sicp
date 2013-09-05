;;2.18 rev2 used since reverse is reserved
(define (rev2 lat)
  (if (null? lat)
      '()
  (append (rev2 (cdr lat))(list(car lat)))))
