;Alternative Stack Data Abstraction

(define (make-stack)(cons 'stack '()))

(define (stack? stack)
  (and (pair? stack)(eq? 'stack (car stack))))

(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "object not a stack")
      (null? (cdr stack))))

(define (insert! stack elt)
  (cond 
    ((not (stack? stack))
     (error "object not a stack"))
    (else
     (set-cdr! stack (cons elt (cdr stack)))
     stack))) ;Return the value of the argument, since it gives us the labeled data structure

(define (delete! stack)
  (if (empty-stack? stack)
      (error "stack underflow - delete")
      (set-cdr! stack (cddr stack))) ;cddr = cdr cdr
  stack)

(define (top stack)
  (if (empty-stack? stack)
      (error "stackunderflow top")
      (cadr stack))) ;cadr = car cdr

;;Better Queue Abstraction
;
;constructor:	(make-queue)	        returns an empty queue
;accessors:	(front-queue q)	        returns the object at the front of the queue. If queue is empty signals error
;mutators:	(insert-queue! q elt)	inserts the elt at the rear of the queue annd returns the modified queue
;               (delete-queue! q)	removes the elt at the front of the queue and returns the modified queue
;operations:	(queue? q)	        tests if the object is a queue
;               (empty-queue? q)	tests if the queue is empty


;; Hidden inside the abstraction
(define (front-ptr q) (cadr q))
(define (rear-ptr q)  (cddr q))
(define (set-front-ptr! q item)
  (set-car! (cdr q) item))
(define (set-rear-ptr! q item)
  (set-cdr! (cdr q) item))

;; Implementation of the abstraction (see above)
(define (make-queue)
  (cons 'queue (cons nil nil)))

(define (queue? q)
  (and (pair? q) (eq? 'queue (car q))))

(define (empty-queue? q)
  (if (not (queue? q))                 ;defensive 
      (error "object not a queue:" q)  ;programming
      (null? (front-ptr q))))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front of empty queue:" q)
      (car (front-ptr q))))

(define (insert-queue! q elt)
  (let ((new-pair (cons elt nil)))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "delete of empty queue:" q))
        (else
         (set-front-ptr! q 
            (cdr (front-ptr q)))
         q)))

;;
(define fill-queue! 
  (lambda (queue elts)
    (cond
      ((null? elts) queue)
      (else
       (insert-queue! queue (car elts))
       (fill-queue! queue (cdr elts)))))) 

