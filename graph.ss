(load "set.ss")

(define (empty-graph) '(() ()))

(define (graph-add-vertex vertex g)
  (graph (cons vertex (graph-vertices g))
         (graph-edges g)))

(define (graph-add-edge edge g)
  (graph (graph-vertices g)
         (cons edge (graph-edges g))))

(define (graph vertices edges)
  (list vertices edges))

(define (graph? item)
  (and (equal? (length item) 2)
       (set? (graph-vertices item))
       (set? (graph-edges item))
       (andmap edge? (graph-edges item))))

(define (graph-vertices g)
  (car g))

(define (graph-edges g)
  (cadr g))

(define (edge origin dest)
  (list origin dest))

(define (edge? item)
  (and (list? item)
       (equal (length item) 2)))