#lang racket

(define (compare p1 p2 n)
  (equal? (p1 n) (p2 n))
  )

(define (p1 n)
  (* n 2)
  )

(define (p2 n)
  (+ n 2)
  )

(display "1-a: ")
(compare p1 p2 2)

;------------------------------------------

(define (make-comparator p1 p2)
  (lambda (n) (equal? (p1 n) (p2 n)))
  )

(display "1-b: ")

(define mc
  (make-comparator p1 p2)
)
(mc 1)

;------------------------------------------

(define comparator
  (make-comparator p1 p2)
  )

(define (test comparator from to)
  (cond
    [(< from to) (if (equal? (p1 from) (p2 from))
          (test comparator (+ from 1) to)
          #f
          )]
    [(= from to) (if (equal? (p1 from) (p2 from))
          #t
          #f
          )]
    )   
  )

(display "1-c: ")
(test comparator 2 2)

;------------------------------------------

(define (make-test from to)
  (lambda (comparator) (test comparator from to))
  )

(define make-test-cp
  (make-test 2 3)
  )

(display "1-d: ")
(make-test-cp comparator)

;------------------------------------------

(define (proc)
  (lambda (n) (even? n))
  )

(define procedure
  (proc)
  )

(define (complement procedure)
  (lambda (n) (not (procedure n)))
  )

(display "2-a: ")
((complement even?) 2)

;------------------------------------------

(define x 5)

(display "2-b [lambda]: ")
((lambda (a b c)
   (+ (* a (* x x)) (* b x) c)
   )
 1 2 3)

(display "2-b [let]: ")
(let ((a 1)
      (b 2)
      (c 3))
  (+ (* a (* x x)) (* b x) c))

;------------------------------------------

(display "2-c: ")
(let* ((x 4) 
      (y (+ x 1))) 
  (+ x y))

;------------------------------------------

(display "2-d [original]: ")
(let ((+ -) 
      (- +) 
      (* /) 
      (/ *)) 
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2))

(display "2-d [rewritten]: ")
(let* ((x 6)
        (y (+ 3 2 5 x))
        (z (* y 3)))
  (/ z 2))

;------------------------------------------

(display "2-e: ")

(define (consequent)
  (display "consequent")
  )

(define (alternative)
  (display "alternative")
  )

(define (iffy test consequent alternative)
  (if test (consequent) (alternative))
  )

(iffy (even? 3) consequent alternative)