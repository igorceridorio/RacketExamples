#lang plai

; Only binop operations implemented

;------------------------------------------

(define-type FAE
  [ num (n number?)]
  
;------------------------------------------
  ;defining the binop type
  [binop (op procedure?) (l FAE?) (r FAE?)]
;------------------------------------------
  [ add (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ fun (param symbol?) (body FAE?)]
  [ app (fun-expr FAE?) (arg-expr FAE?)])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (param symbol?)
             (body FAE?)
             (ds DefrdSub?)])

;------------------------------------------
;checking if both arguments are numbers
(define (numbersBinop op l r)
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else (error "arguments must be numbers")]
   )
 )
;------------------------------------------

;------------------------------------------
; defining the binary operations (binops)
(define binops
  (list (cons '+ (lambda (l r) (numbersBinop + l r)))
        (cons '- (lambda (l r) (numbersBinop - l r)))
        (cons '* (lambda (l r) (numbersBinop * l r)))
        (cons '/ (lambda (l r) (numbersBinop (lambda (l r) (if (= 0 r) (error "division by zero") (/ l r))) l r)))
        )
  )
;------------------------------------------

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub â†’ FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-ds))]))

;; num+ : numV numV âˆ’â†’ numV
(define (num+ n1 n2)
  ( numV (+ (numV-n n1) (numV-n n2))))

;------------------------------------------
; defining reserved symbols

(define reserved-symbols
  (set-union (set 'if0 'with 'fun)
             (list->set (dict-keys binops)))
 )
;------------------------------------------

;=====================================================================================
; INTERPRETER
;=====================================================================================
;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ add (l r) (num+ (interp l ds) (interp r ds))]

;------------------------------------------
    ; adding the interpreter for the binops
    [binop (op l r) (op (interp l ds) (interp r ds))]
;------------------------------------------
    
    [ id (v) (lookup v ds)]
    [ fun (bound-id bound-body)
          ( closureV bound-id bound-body ds)]
    [ app (fun-expr arg-expr)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    ( aSub (closureV-param fun-val)
                           (interp arg-expr ds)
                           (closureV-ds fun-val))))]))
;=====================================================================================
; PARSER
;=====================================================================================
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)     
     (case (first sexp)       
       [(+) (parseBinop sexp)]
       [(-) (parseBinop sexp)]
       [(*) (parseBinop sexp)]
       [(/) (parseBinop sexp)]
              ;[(with) (with (first (second sexp))
              ;              (parse (second (second sexp)))
              ;             (parse (third sexp)))
              ;        ]
       [(fun) (fun 
               (first (second sexp))
               (parse (third sexp))
               )
              ]
       [else
        (cond ((list? (first sexp))
                ;modify by parsing the following to handle multiple params
               (app (parse (first sexp)) (parse (second sexp)))))
        ]
       )]))

;=====================================================================================

;------------------------------------------
; defining the parser for the binop operations

(define (parseBinop sexp)
  (if (= 3 (length sexp))
      (binop (dict-ref binops (first sexp))
      (parse (second sexp))
      (parse (third sexp)))
  (error "binary op must have 2 arguments"))
  )
;------------------------------------------

; TEST EXAMPLES

(interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )
(interp (parse '{+ (+ 5 1) 2}) (mtSub) )
(interp (parse '{- 6 (* 2 2)}) (mtSub) )
(interp (parse '{* 2 (+ 2 (- 8 2))}) (mtSub) )
(interp (parse '{/ 6 0}) (mtSub) )
