(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (define (addFirst first rst) 
    (append (list first) rst)
  ) 
  (if (null? rests) 
    nil 
    (append (list (addFirst first (car rests))) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  'replace-this-line)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerateHelper start s) 
    ; if s is nul 
    (if (null? s)
      ; return nil
      nil 
      ; else enumerate that value and the rest
      (append (list (list start (car s))) (enumerateHelper (+ start 1) (cdr s))) 
    ) 
  ) 
  (enumerateHelper 0 s) 
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cons-all 1 '((2 4) (1 5) (6 3)))
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
   
)
;  ; END PROBLEM 18
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
         'replace-this-line
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         'replace-this-line
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           'replace-this-line
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         'replace-this-line
         ; END PROBLEM 19
         )))
