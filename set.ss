(define (set ls)
  (union ls '()))

(define (set? item)
  (and (list? item)
       (equal? (set item) item)))

(define (union set1 set2)
  (cond
   ((empty? set1) set2)
   ((member (car set1) set2)
    (union (cdr set1)
           set2))
   (else
    (union (cdr set1)
           (cons (car set1) set2)))))
