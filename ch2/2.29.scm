;2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))
;b.
(define (total-weight mobile)
  (sum-branch mobile))

(define (sum-branch mobile)
  (cond
   ((and (number? (branch-structure (left-branch mobile)))(number? (branch-structure(right-branch mobile))))
   (+ (branch-structure (left-branch mobile))(branch-structure (right-branch mobile))))
   ((number? (branch-structure(left-branch mobile)))
   (+ (branch-structure (left-branch mobile))(sum-branch (right-branch mobile))))
   ((number? (branch-structure(right-branch mobile)))
   (+ (branch-structure (right-branch mobile))(sum-branch (left-branch mobile))))
   (else
    (+ (sum-branch (left-branch mobile))(sum-branch (right-branch mobile))))))

(define m_0 (make-mobile (make-branch 1 1) (make-branch 1 1)))
(define m_1 (make-mobile m_0 (make-branch 1 2)))
(define m_2 (make-mobile (make-branch 1 3) (make-branch 1 2)))
(define m_3 (make-mobile m_2 m_1))

;c. Write predicate balanced

 