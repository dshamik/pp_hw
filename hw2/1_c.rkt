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

(define (encode-with-lengths lst)
  (define (helper counter previous lengths lst)
    (cond
      [
        (empty? lst) (cons counter lengths)
      ]
      [
        (= (first lst) previous) (helper (+ counter 1) previous lengths (rest lst))
      ]
      [
        else (helper 1 (first lst) (cons counter lengths) (rest lst))
      ]
    )
  )
  (reverse (helper 0 1 '() (delete-zeros lst)))
)

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) ; '(2 1 3 2)