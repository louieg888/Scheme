(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (define (add_first first rst) 
    (append (list first) rst)
  ) 
  (if (null? rests) 
    nil 
    (append (list (addFirst first (car rests))) (cons-all first (cdr rests)))
  )
)

(define (zip pairs)
  (cons (map car pairs) (map car (map cdr pairs)))
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerate_helper start s) 
    ; if s is nul 
    (if (null? s)
      ; return nil
      nil 
      ; else enumerate that value and the rest
      (append (list (list start (car s))) (enumerate_helper (+ start 1) (cdr s)))
    ) 
  ) 
  (enumerate_helper 0 s) 
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (define (helper tot den) 
    (cond 
      ; if total == 0
      ((= tot 0) (list (list (car den))))
      ; (append does nothing 
      ((< tot 0) nil) 
      ; if the total is still greater than 0 and you have no denoms left, no combos
      ((null? den) nil) 
      ; if the current denom is bigger than total, then call helper on rest. 
      ((> (car den) tot) (list-change tot (cdr den)))
      ; if the first denom is equal to the total, 
      ; ((= (car denoms) total) )
      ; this means you have combos. 
      (else 
        (append   
          (cons-all (car den) (helper (- tot (car den)) den))
          (helper tot (cdr den))
        )
      )
    )
  )
  (define output (helper total denoms))
  (define (removeLast lst) 
    ;(display lst)
    ;(newline)
    (if (eq? '() (cdr lst)) 
      '() 
      (cons (car lst) (removeLast (cdr lst))) 
    )
  ) 
  (display output)

  (define first (list 10 10 10 10 10 10))
  ; (display first)
  (newline)
  (define second (cadr output))
  ; (display second) 
  (newline)
  
  ;(display (removeLast first))
;  (display (cdr first))
;  (newline)
;  (display (car first))
;  (newline)
;  (display (car (cdr first)))
;  (newline)
;  (display (cdr (cdr first)))
;  (display (eq? '() (cddr first)))
;  (newline)
;  (display first)
;  (removeLast '(5 5 5))
;  (removeLast '(10 10))
;  (define (listMap proc lst) 
;    (if (null? lst) 
;      nil
;      (append (list (proc (car lst))) (listMap proc  (cdr lst))) 
;  (cond 
;    ; if total == 0
;    ((= total 0) (list (list (car denoms))))
;    ; (append does nothing 
;    ((< total 0) nil) 
;    ; if the total is still greater than 0 and you have no denoms left, no combos
;    ((null? denoms) nil) 
;    ; if the current denom is bigger than total, then call helper on rest. 
;    ((> (car denoms) total) (list-change total (cdr denoms)))
;    ; if the first denom is equal to the total, 
;    ; ((= (car denoms) total) )
;    ; this means you have combos. 
;    (else 
;      (append   
;        (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
;        (list-change total (cdr denoms))
;      )
;    )
;  ) 
  ; (listMap removeLast output)
  ; (map removeLast output)
  ; (listMap (lambda (lt) (append lt (list 1))) '((1 2) (1 3) (4 5)))
)
  

; END PROBLEM 18
;
;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cond
	    ((null? (cdr (cddr expr))) (cons form (cons params (cons (car body) nil))))
	    (else  (cons form (cons params (cons (car body) (cons (let-to-lambda (cadr (cddr expr))) nil)))))
	    )
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
	   ; BEGIN PROBLEM 19
	   (cons (cons 'lambda (list (car (zip values)) (let-to-lambda (car body)))) (map let-to-lambda (cdr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (list (car expr) (let-to-lambda (cadr expr)) (let-to-lambda (car (cddr expr))))
         ; END PROBLEM 19
         )))
