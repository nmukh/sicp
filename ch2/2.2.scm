;;2.2
(define (make-point x y)
  (list x y))

(define x-point car)
(define y-point cadr)

(define (segment p1 p2)
  (cons p1 p2))

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (midpoint-segment segment)
  (make-point (/(- (car (cdr segment))(car(car segment)))2)
              (/(- (cadr (cdr segment))(cadr(car segment)))2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))