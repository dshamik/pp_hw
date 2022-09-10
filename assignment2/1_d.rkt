#lang slideshow

(define (binary-odd? lst)
  (cond
    [(empty? (rest lst)) (= (first lst) 0)]
    [else (binary-odd? (rest lst))]
  )
)