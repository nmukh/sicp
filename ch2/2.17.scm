(define (last-pair list1)
  (cond ((null? list1)'())
  ((null?(cdr list1))(car list1))
  ((last-pair (cdr list1)))))


