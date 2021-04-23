#lang racket
 
(provide (all-defined-out))

(define (pow x y)
  (if (= y 0)
    1
    (* x (pow x (- y 1)))))

(define (sum xs)
  (if (null? xs)
    0
    (+ (car xs) (sum (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(define (map f xs)
  (if (null? xs)
    null
    (cons (f (car xs)) (map f (cdr xs)))))

; Cond is better style for nested conditionals
(define (suml xs)
  (cond [(null? xs) 0]
	[(number? (car xs)) (+ (car xs) (suml (cdr xs)))]
	[#t (+ (suml (car xs)) (suml (cdr xs)))]))

(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (if (mcar th)
    (mcdr th)
    (begin (set-mcar! th #t)
	   (set-mcdr! th ((mcdr th)))
	   (mcdr th))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* 2 x)))))])
    (lambda () (f 2))))

(define fibonacci
  (letrec([memo null]
	  [f (lambda (x)
	       (let ([ans (assoc x memo)])
		 (if ans
		   (cdr ans)
		   (let ([new-ans (if (or (= x 1) (= x 2))
				    1
				    (+ (f (- x 1))
				       (f (- x 2))))])
		     (begin
		       (set! memo (cons (cons x new-ans) memo))
		       new-ans)))))])
    f))

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3) (if e1 e2 e3)]))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e) 
     (mcons #f (lambda () e))]))
