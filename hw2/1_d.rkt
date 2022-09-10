#lang slideshow

(define (decrement lst)
  (define (helper acc lst)
    (cond

      [
        (empty? lst) (list 0)
      ]
      [
        (equal? (first lst) 1) (append acc (cons 0 (rest lst)))
      ]
      [
        else (helper (cons 1 acc) (rest lst))
      ]
    )
  )
  reverse (helper empty (reverse lst))
)

(decrement '(1 0 1 1 0)) ; '(1 0 1 0 1)
(decrement '(0)) ; '(0)