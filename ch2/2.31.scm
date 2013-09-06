;2.31
(define (square x) (* x x))

(define (tree-map f tree)
  (map (lambda (subtree)
       (if (pair? subtree) 
           (tree-map f subtree)
           (f subtree)))
       tree))
         
(define (square-tree tree)(tree-map square tree))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(tree-map square t)

