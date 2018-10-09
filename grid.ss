(load "graph.ss")
(load "set.ss")

(define (grid-max grid)
  (grid-apply max grid))

(define (grid-min grid)
  (grid-apply min grid))

(define (grid-apply list-func grid)
  (apply list-func (map (lambda (row) (apply list-func row)) grid)))

(define (grid-map elem-func grid)
  (map (lambda (row) (map elem-func row)) grid))

(define (grid-andmap elem-func grid)
  (andmap (lambda (row) (andmap elem-func row)) grid))

(define (grid->graph grid)
  (define (maybe-make-edge row col dir)
    (case dir
      ('right
       (if (< (+ col 1) (grid-num-cols grid))
           (edge (make-vertex row col)
                 (make-vertex row (+ col 1)))
           #f))
      ('up
       (if (>= (- row 1) 0)
           (edge (make-vertex row col)
                 (make-vertex (- row 1) col))
           #f))
      ('left
       (if (>= (- col 1) 0)
           (edge (make-vertex row col)
                 (make-vertex row (- col 1)))
           #f))
      ('down
       (if (< (+ row 1) (grid-num-rows grid))
           (edge (make-vertex row col)
                 (make-vertex (+ row 1) col))
           #f))
      (else #f)))

  (define (make-vertex row col)
    (list (grid-ref grid row col)
          (cons row col)))

  (let loop ((row 0) (col 0))
    (cond
     ((>= row (grid-num-rows grid))
      (empty-graph))
     ((>= col (grid-num-cols grid))
      (loop (+ row 1) 0))
     (else
      (let* ((graph-rest
              (loop row (+ col 1)))
             (graph-with-vertex
              (graph-add-vertex (make-vertex row col)
                                graph-rest))
             (edges
              (filter (lambda (x) x)
                      (map (lambda (dir)
                             (maybe-make-edge row col dir))
                           '(right up down left)))))
        (fold-right graph-add-edge graph-with-vertex edges))))))

(define (grid-num-cols grid)
  (if (equal? (grid-num-rows grid) 0)
      0
      (length (grid-row-ref grid 0))))

(define (grid-num-rows grid)
  (length grid))

(define (grid-ref grid row col)
  (list-ref (grid-row-ref grid row) col))

(define (grid-row-ref grid row)
  (list-ref grid row))
