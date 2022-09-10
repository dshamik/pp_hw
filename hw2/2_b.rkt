#lang slideshow

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

(define (alternating-sum lst)
  (helper 0 1 lst)
)

(alternating-sum (list 1 2 3 4 5)) ; 3

; (helper 0 1 (list 1 2 3 4 5))
; (helper (+ 0 (* (first (list 1 2 3 4 5)) 1)) (* 1 -1) (rest(list 1 2 3 4 5)))
; (helper (+ 0 (* 1 1)) (* 1 -1) (rest(list 1 2 3 4 5)))
; (helper (+ 0 1) (* 1 -1) (rest(list 1 2 3 4 5)))
; (helper 1 (* 1 -1) (rest(list 1 2 3 4 5)))
; (helper 1 -1 (rest(list 1 2 3 4 5)))
; (helper 1 -1 '(2 3 4 5))
; (helper (+ 1 (* (first '( 2 3 4 5)) -1)) (* -1 -1) (rest '(2 3 4 5)))
; (helper (+ 1 (* 2 -1)) (* -1 -1) (rest '(2 3 4 5)))
; (helper (+ 1 -2) (* -1 -1) (rest '(2 3 4 5)))
; (helper -1 (* -1 -1) (rest '(2 3 4 5)))
; (helper -1 1 (rest '(2 3 4 5)))
; (helper -1 1 '(3 4 5))
; (helper (+ -1 (* (first '(3 4 5)) 1)) (* 1 -1) (rest '(3 4 5)))
; (helper (+ -1 (* 3 1)) (* 1 -1) (rest '(3 4 5)))
; (helper (+ -1 3) (* 1 -1) (rest '(3 4 5)))
; (helper 2 (* 1 -1) (rest '(3 4 5)))
; (helper 2 -1 (rest '(3 4 5)))
; (helper 2 -1 '(4 5))
; (helper (+ 2 (* (first '(4 5)) -1)) (* -1 -1) (rest '(4 5))
; (helper (+ 2 (* 4 -1)) (* -1 -1) (rest '(4 5))
; (helper (+ 2 -4) (* -1 -1) (rest '(4 5))
; (helper -2 (* -1 -1) (rest '(4 5))
; (helper -2 1 (rest '(4 5))
; (helper -2 1 '(5)
; (helper (+ -2 (* (first '(5)) 1)) (* 1 -1) (rest '(5)))
; (helper (+ -2 (* 5 1)) (* 1 -1) (rest '(5)))
; (helper (+ -2 5) (* 1 -1) (rest '(5)))
; (helper 3 (* 1 -1) (rest '(5)))
; (helper 3 -1 (rest '(5)))
; (helper 3 -1 '())
; 3