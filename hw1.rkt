#lang racket

(define (miles-to-kilometers miles)
  (* miles 1.609)
  )

(display "1-a: ")
(miles-to-kilometers 2)
;----------------------------------

(define (kilometers-to-miles km)
  (* km 0.6215)
  )

(display "1-b: ")
(kilometers-to-miles 1.6)
;----------------------------------

(define (foo x y)
  (+ x y (- x))
  )

(display "2-a: ")
(foo (- 2) 4)
;----------------------------------

(define (a b c d)
  (e b c d)
  )
(define f +)
(define e f)

(display "2-b: ")
(a (+ 3 2) (* 4 5) 3)
;----------------------------------

(define (g h i j)
   ((l (m h)) i j))

(define (l g) 
   g)

(define (m g)
   (if (positive? g) 
       +
       -))

(display "2-c: ")
(g 1 (+ 2 3) 4)
;----------------------------------

(define (baz n)
  (define (bar a n)
    (if (= a 14)
        n
        (bar (+ a 1) (+ (* a (+ a 2)) n))
        )
    )
  (bar 3 n)
  )
(display "3: ")
(baz 2)
;----------------------------------

(define sum 0)

(define (sum-range from to)
  (cond
    [(< from to) (+ from (sum-range (+ from 1) to))]
    [(< to from) (+ to (sum-range (+ to 1) from))]
    [(= from to) to]
  )
  )

(display "4: ")
(sum-range 4 2)