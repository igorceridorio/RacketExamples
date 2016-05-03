#lang racket

(define (reverse-digits rev n)
  (if (<= n 0)
     rev
     (reverse-digits (+ (* rev 10) (modulo n 10)) (quotient n 10))
     )
  )

(display "1-a: ")
(reverse-digits 0 12345)

;------------------------------------------

(define (apnd a b)
  (flatten (cons a b))
  )

(define (create-list n [list '()])
  (if (<= n 0)
      list
      (create-list (quotient n 10) (apnd (remainder n 10) list))
   )
  )

(define (same-digits? a b)
  (equal? (sort (create-list a) <) (sort (create-list b) <))
  )

(display "1-b: ")
(same-digits? 133042 42013)

;------------------------------------------

(define (von-neumann previous digits)
  (remainder (quotient (* previous previous) (expt 10 (/ digits 2))) (expt 10 digits))
  )

(display "2: ")
(von-neumann 5772156649 10)

;------------------------------------------

(define (is-prime? n [counter 2])
  (if (= n 1)
      #t
      (if (= counter n)
      #t
      (cond
        [(= (remainder n counter) 0) #f]
        [(> (remainder n counter) 0) (is-prime? n (+ counter 1))]
        )
     )
   )
   )

(define (mersenne n)
  (if (is-prime? n)
      (if (is-prime? (- (expt 2 n) 1))
          n
          (mersenne (+ n 1))
          )
      (mersenne (+ n 1))
      )
  )

(display "3: ")
(mersenne 18)



