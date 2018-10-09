(define (grid-max grid)
  (grid-apply max grid))

(define (grid-min grid)
  (grid-apply min grid))

(define (grid-apply list-func grid)
  (apply list-func (map (lambda (row) (apply list-func row)) grid)))

(define (grid-map elem-func grid)
  (map (lambda (row) (map elem-func row)) grid))

(define (grid-ref grid row col)
  (list-ref (list-ref grid row) col))
