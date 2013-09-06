;2.37
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (accumulate
   cons
   '()
  (map (lambda(row)
        (dot-product row v)) m)))
         

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 1 1) (1 1 1) (1 1 1) (1 1 1)))
(define v '(1 1 1 1))
(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n cons '() mat))

;;Need to improve matrix-*matrix
(define (matrix-*-matrix m n)
  (accumulate
   cons
   '()
  (let ((cols (transpose n)))
    (map (lambda (row col)
           (dot-product row col)) m cols))))

