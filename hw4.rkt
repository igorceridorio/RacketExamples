#lang racket

;------------------------------------------

(display "1-a: [version 1]")

(define (contains1? my_list object)
  (if (null? my_list) #f (if (equal? (car my_list) object) #t (contains1? (cdr my_list) object)))
  )

(contains1? (list 1 2 3) 0)

;------------------------------------------

(display "1-a: [version 2]")

(define (accumulate operator null list)
  (if (null? list)
      null
      (operator (car list)
                (accumulate operator null (cdr list)))))

(define (contains2? my_list object)
  (accumulate (lambda (x y) (or x y) )
              #f
              (map (lambda (x) (equal? x object)) my_list))
  )

(contains2? (list 1 2 3) 0)

;------------------------------------------

(display "1-b: [version 1]")

(define (remove1 object my_list)
  (if (null? my_list)
      '()
      (if (equal? object (car my_list))
          (remove1 object (cdr my_list))
          (cons (car my_list) (remove1 object (cdr my_list)))
       )
      )
  )

(remove1 2 (list 1 2 3 2 1))

;------------------------------------------

(display "1-b: [version 2]")

(define (remove2 object list)
   (accumulate (lambda (item rest)
                 (if (equal? object item)
                     rest
                     (cons item rest)
                     )
                 )
		null
		list))

(remove2 2 (list 1 2 3 2 1))

(display "1-b: [version 2] exercise example: ")

(let ((lst (list 1 2 3)))
   (and (equal? (list 1 3) (remove2 2 lst))
        (equal? lst (remove2 4 lst))))

;------------------------------------------

;; 1 + (2 / (3 * 5 + 1)) + (-4)
(define expr
   (list +
         1
         (list /
               2
               (list +
                     (list * 3 5)
                     1))
         (list - 0 4)))

;expr

(display "2-a: ")

(define operators (list + - * /))

(define (member? item seq)
  (sequence-ormap (lambda (x)
                    (equal? item x))
                  seq))

(define (operator? object)
  (member? object operators)
  )

(operator? (car expr))

;------------------------------------------

(display "2-b: ")

(define (lastt lst)
  (if (equal? lst #f)
      #f
      (if (= (length lst) 1)
      (car lst)
      (lastt (cdr lst)))))

(define (null-val operator)
  (lastt (assoc operator (list (list + 0) (list - 0) (list * 1) (list / 1))))
  )

(null-val *)

;------------------------------------------

(define (expression? object)
  (if (number? object)
      #t
     (and (list? object) (> (length object) 2) (operator? (car object)) (andmap expression? (cdr object)))
     )
  ) 

(display "2-c: [expr]")
(expression? expr)

(display "2-c: [(list 1 + 2)]")
(expression? (list 1 + 2))

(display "2-c: [(list + 1)]")
(expression? (list + 1))

;------------------------------------------

(display "2-d: [count-operators]")

(define (count-operators expr)
  (define x (flatten expr))
  x
  (apply + (map (lambda (y)
         (if (operator? y) 1 0))
       x))
 )

(count-operators expr)

;------------------------------------------

(display "2-d: [count-primitive-operands]")

(define (count-primitive-operands expr)
  (define x (flatten expr))
  x
  (apply + (map (lambda (y)
         (if (number? y) 1 0))
       x))
 )

(count-primitive-operands expr)

;------------------------------------------

(display "2-e: ")

(define (evaluate expr)
  (if (number? expr)
      expr
      (apply (car expr) (map evaluate (cdr expr))))
  )

(evaluate expr)
