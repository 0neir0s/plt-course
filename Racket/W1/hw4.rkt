
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride)
  (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
	[(null? xs) (error "list-nth-mod: empty list")]
	[#t (let* ([i (remainder n (length xs)) ])
	      (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (n) 
		(cons (if (= (remainder n 5) 0) (- 0 n) n)
		      (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([pics (list "dan.jpg" "dog.jpg")]
	   [f (lambda (n) (cons (list-nth-mod pics n) 
				(lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  (letrec ([f (lambda () (cons (cons 0 (car (s)))
			       (stream-add-zero (cdr (s)))))])
    f))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (i) (cons (cons (list-nth-mod xs i) 
				      (list-nth-mod ys i))
				  (lambda () (f (+ i 1) ))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
	   [f (lambda (c) 
		(if (>= c l) #f
		  (let ([val (vector-ref vec c)])
		    (cond [(not (pair? val)) (f (+ c 1))]
			  [(equal? v (car val)) val]
			  [#t (f (+ c 1))]))))])
    (f 0)))
			
(define (cached-assoc xs n)
  (letrec ([pos 0]
	   [cache (build-vector n (lambda (x) #f))]
	   [f (lambda (v)
		(letrec ([vecval (vector-assoc v cache)]
			 [checker (lambda () 
				    (let* ([aval (assoc v xs)])
				      (and aval (begin
						  (print aval)
						  (vector-set! cache pos aval)
						  (set! pos (remainder (+ pos 1) n))
						  aval))))])
		  (or vecval (checker))))])
    f))
