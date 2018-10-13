(define (set ls)
  (set-union ls '()))

(define (set? item)
  (and (list? item)
       (equal? (set item) item)))

(define (set-intersect set1 set2)
  (set-filter (lambda (elem)
                (member elem set2))
              set1))

(define (set-union set1 set2)
  (cond
   ((empty? set1) set2)
   ((member (car set1) set2)
    (set-union (cdr set1)
               set2))
   (else
    (set-union (cdr set1)
               (cons (car set1) set2)))))

(define (set-difference set1 set2)
  (set-filter (lambda (elem)
                (not (member elem set2)))
              set1))

(define set-filter filter)
