#lang slideshow

(define (power-of-two n)
  (expt 2 n)
)

(define (binary-to-decimal lst)
  (define (helper idx summ lst)
    (cond
      [
        (empty? lst) summ
      ]
      [
        else (helper (+ idx 1) (+ summ (* (power-of-two idx) (first lst))) (rest lst))
      ]
    )
  )
  (helper 0 0 (reverse lst))
)

(binary-to-decimal '(1 0 1 1 0)) ; 22
