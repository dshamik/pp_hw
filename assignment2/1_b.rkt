#lang slideshow

(define (delete-zeros lst)
  (cond
    [
      (empty? lst) lst
    ]
    [
      (equal? (first lst) 0) (delete-zeros (rest lst))
    ]
    [
      else lst
    ]
  )
)

(define (count-zeros lst)
  (define (helper cntr lst)
    (cond
      [
        (empty? lst) cntr
      ]
      [
        (= (first lst) 1) (helper cntr (rest lst))
      ]
      [
        else (helper (+ cntr 1) (rest lst))
      ]
    )
  )
  (helper 0 (delete-zeros lst))
)

(count-zeros '(0 0 0 1 0 1 1 0)) ; 2

