#lang slideshow

(define (alternating-sum lst)
  (define (helper sign lst)
    (cond
      [
        (empty? lst) 0
      ]
      [
        else (+ (helper (* sign -1) (rest lst)) (* sign (first lst)))
      ]
    )
  )
  (helper 1 lst)
)

(alternating-sum (list 6 2 4 1 3 9)) ; 1