#lang slideshow

(define (alternating-sum-c lst)
  (define (helper summ sign lst)
    (cond
      [
        (empty? lst) summ
      ]
      [
        else (helper (+ summ (* (first lst) sign)) (* sign -1) (rest lst))
      ]
    )
  )
  (helper 0 1 lst)
)

(alternating-sum-c (list 6 2 4 1 3 9)) ; 1