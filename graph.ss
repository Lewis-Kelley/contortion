(load "set.ss")

(define (graph->connected-subgraphs g)
  (let loop ((g g)
             (results '()))
    (if (null? (graph-vertices g))
        results
        (let* ((root-vertex (car (graph-vertices g)))
               (subgraph (connected-subgraph g root-vertex))
               (remaining (graph-difference g subgraph)))
          (loop remaining (cons subgraph results))))))

(define (graph-difference graph1 graph2)
  (let* ((both-vertices (set-intersect (graph-vertices graph1)
                                    (graph-vertices graph2)))
         (remaining-vertices (set-difference (graph-vertices graph1)
                                          both-vertices))
         (remaining-edges (graph-filter-edges
                           (lambda (e)
                             (not (or (member (edge-origin e) both-vertices)
                                      (member (edge-dest e) both-vertices))))
                           graph1)))
    (graph remaining-vertices remaining-edges)))

(define (connected-subgraph g vertex)
  (let loop ((queued-edges (graph-vertex-outgoing-edges g vertex))
             (checked-vertices (list vertex))
             (checked-edges (list)))
    (if (null? queued-edges)
        (graph checked-vertices checked-edges)
        (let* ((next-edge (car queued-edges))
               (next-destination (edge-dest next-edge)))
          (if (member next-destination checked-vertices)
              (loop (cdr queued-edges)
                    checked-vertices
                    (cons next-edge checked-edges))
              (loop (append
                     (graph-vertex-outgoing-edges g next-destination)
                     (cdr queued-edges))
                    (cons next-destination checked-vertices)
                    (cons next-edge checked-edges)))))))

(define (graph-vertex-edges g vertex)
  (set-union (graph-vertex-incoming-edges g vertex)
             (graph-vertex-outgoing-edges g vertex)))

(define (graph-vertex-incoming-edges g vertex)
  (graph-filter-edges (lambda (e)
                        (equal? (edge-dest e)
                                vertex)) g))

(define (graph-vertex-outgoing-edges g vertex)
  (graph-filter-edges (lambda (e)
                        (equal? (edge-origin e)
                                vertex)) g))

(define (graph-filter-edges pred? g)
  (let ((all-edges (graph-edges g)))
    (set-filter pred? all-edges)))

(define (graph-add-vertex vertex g)
  (graph (cons vertex (graph-vertices g))
         (graph-edges g)))

(define (graph-add-edge edge g)
  (graph (graph-vertices g)
         (cons edge (graph-edges g))))

(define (empty-graph)
  (graph '() '()))

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

(define edge-origin car)
(define edge-dest cadr)
