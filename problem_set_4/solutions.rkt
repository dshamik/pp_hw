#lang slideshow

; Exercise 1 (a)

(define (replicate n symbols)
    (cond
        [
             (equal? n 1)
             (list symbols)
        ]
        [
            else
            (cons symbols (replicate (- n 1) symbols))
        ]
    )
)
(displayln "Exercise 1 (a)")

(displayln "(replicate 10 'a)")
(replicate 10 'a)

(displayln "(replicate 3 '(1 . 2))")
(replicate 3 '(1 . 2))

(displayln "")


; Exercise 1 (b)

; I could not impliment return of prefix and suffix with dot between :(

(define (split lst split-point)
    (define (helper result)
        (list (cons (car lst) (car result)) (cadr result))
    )
    (cond
        [
            (empty? lst)
            (list '() '())
        ]
        [
            (equal? split-point 0)
            (list '() lst)
        ]
        [
            else
            (helper (split (cdr lst) (- split-point 1)))
        ]
    )
)

(displayln "Exercise 1 (b)")

(displayln "(split '(1 2 3 4 5) 2)")
(split '(1 2 3 4 5) 2)

(displayln "(split '(a b c d) 4)")
(split '(a b c d) 4)

(displayln "(split '(a b c) 4)")
(split '(a b c) 4)

(displayln "(split '(a b c) 0)")
(split '(a b c) 0)

(displayln "")


; Exercise 1 (c)

(define (chunks lst len)
    (define (helper result)
        (cons (car result) (chunks (cadr result) len))
    )
    (cond
        [
            (equal? '() lst)
            lst
        ]
        [
            else
            (helper (split lst len))
        ]
    )
)

(displayln "Exercise 1 (c)")

(displayln "(chunks '(1 2 3 4 5) 2)")
(chunks '(1 2 3 4 5) 2)

(displayln "(chunks '(a b c d e f) 3)")
(chunks '(a b c d e f) 3)

(displayln "")


; Exercise 1 (d)

(define (windows lst len)
    (cond
        [
            (equal? '() lst)
            lst
        ]
        [
            (<= (length lst) len)
            (list lst)
        ]
        [
            else
            (cons (car (split lst len)) (windows (cdr lst) len))
        ]
    )
)

(displayln "Exercise 1 (d)")

(displayln "(windows '(1 2 3 4 5) 2)")
(windows '(1 2 3 4 5) 2)

(displayln "(windows '(a b c d e) 3)")
(windows '(a b c d e) 3)

(displayln "")


; Exercise 3 (a)

(define (max lst)
    (foldl (lambda (elmnt mx) (cond [(> elmnt mx) elmnt] [else mx])) 0 lst)
)

(displayln "Exercise 3 (a)")

(displayln "(max '(1 5 3 6 2 0))")
(max '(1 5 3 6 2 0))

(displayln "")


; Exercise 3 (b)

(define (second-max lst)
    (define (helper element secondmax)
        (cond
            [
                (>= element (car secondmax))
                (cons element (car secondmax))
            ]
            [
                else
                secondmax
            ]
        )
    )
    (cdr (foldl helper (list 0 0) lst))
)

(displayln "Exercise 3 (b)")

(displayln "(second-max '(1 5 3 6 2 0))")
(second-max '(1 5 3 6 2 0))

(displayln "")


; Exercise 3 (c)

(define (top-3 lst)
    (define (helper element top)
        (cond
            [
                (>= element (car top))
                (list element (car top) (cadr top))
            ]
            [
                (>= element (cadr top))
                (list (car top) element (cadr top))
            ]
            [
                (> element (caddr top))
                (list (car top) (cadr top) element)
            ]
            [
                else
                top
            ]
        )
    )
    (reverse(foldl helper (list 0 0 0) lst))
)

(displayln "Exercise 3 (c)")

(displayln "(top-3 '(5 3 6 2 8 1 0))")
(top-3 '(5 3 6 2 8 1 0))

(displayln "")


; Exercise 3 (d)

; Next task was really confusing and entangled, pls do not make next exercises so hard to imagine like this one.........

(define (group lst)
    (define (helper-1 element groups+group+element)
        (cond
            [
                (equal? element (caddr groups+group+element))
                (list (car groups+group+element) (cons element (cadr groups+group+element)) element)
            ]
            [
                else
                (list (cons (cadr groups+group+element) (car groups+group+element)) (list element) element)
            ]
        )
    )
    (define (helper-2 h)
            (foldl cons '() (cons (cadr h) (car h)))
    )
    (define (helper-3 fst)
        (helper-2 (foldl helper-1 (list '() (list fst) fst) (cdr lst)))
    )
    (helper-3 (car lst))
)

(displayln "Exercise 3 (d)")

(displayln "(group '(a b b c c c b a a))")
(group '(a b b c c c b a a))

(displayln "")


; Exercise 3 (e)

(define (cumulative-sums lst)
    (define (helper-1 element sums+last)
        (define (helper-2 sums)
            (cons (cons sums (car sums+last)) sums)
        )
        (helper-2 (+ element (cdr sums+last)))
    )
    (foldl cons '() (car (foldl helper-1 (cons (list 0) 0) lst)))
)

(displayln "Exercise 3 (d)")

(displayln "(cumulative-sums '(1 2 3 4 5))")
(cumulative-sums '(1 2 3 4 5))