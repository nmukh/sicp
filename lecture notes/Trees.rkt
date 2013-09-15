;General operations on trees
(define (leaf? x)
  (not (pair? x)))

(define(tree-manip leaf-op init merge tree)
  (if (null? tree)
      init
      (if (leaf? tree)
          (leaf-op tree)
          (merge (tree-manip leaf-op init merge (car tree))
                 (tree-manip leaf-op init merge (cdr tree))))))

;Examples
(tree-manip (lambda(x)(* x x)) '() cons '(1(2 (3 4) 5) 6)) ;square leaves
(tree-manip (lambda(x)1) 0 + '(1(2 (3 4) 5) 6)) ;count leaves
(tree-manip (lambda(x)x) '() (lambda(a b)(append b (list a)))'(1(2 (3 4) 5) 6)) ;deep reverse


;;Simple Search - unordered collection of entries
(define (find1 compare query set )
  (cond
    ((null? set) 'not-here)
    ((compare query (car set))
     (car set))
    (else (find1 compare query (cdr set)))))
; order of growth is linear -if not there: n, if there: n/2
;;insertion: constnt cost, deletion: linear cost

;;Ordered collection
(define (find2 less? same? query set)
  (cond ((null? set) 'not-here)
        ((less? query (car set)) 'not-here)
        ((same? query (car set)) (car set))
        (else (find2 less? same? query (cdr set)))))
;;order of growth is linear - if not there: n/2, if there: n/2
;;insertion and deletion: constant cost


;Binary tree
(define (find3 less? same? query tree)
  (cond ((null? tree) 'not-here)
        ((same? query (entry tree))(entry tree))
        ((less? query (entry tree))
         (find3 less? same? query (left tree)))
        (else (find3 less? same? query (right tree)))))

;;order of growth: log n
;;
