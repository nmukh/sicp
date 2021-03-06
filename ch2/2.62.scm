;Sets as ordered list O(n/2)
(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set? x (cdr set)))))

;;O(2n) growth
(define (intersection-set set1 set2)
  (if (or (null? set1)(null? set2))
      '()
  (let ((x1 (car set1))(x2 (car set2)))
    (cond ((= x1 x2)
           (cons x1 (intersection-set (cdr set1)
                                      (cdr set2))))
          ((< x1 x2)
           (intersection-set (cdr set1) set2))
          ((< x2 x1)
           (intersection-set set1 (cdr set2)))))))

;2.62
  
 (define (union-set set1 set2)
   (cond
     ((and (null? set1)(null? set2))
         '())
     ((null? set1) set2)
     ((null? set2) set1)
     ((= (car set1)(car set2))(cons (car set1)(union-set (cdr set1)(cdr set2))))
     ((< (car set1)(car set2))(cons (car set1)(union-set (cdr set1) set2)))
     ((> (car set1)(car set2))(cons (car set2)(union-set set1 (cdr set2))))))
     

(union-set '(1 2 3) '(2 3 4))