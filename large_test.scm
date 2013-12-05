(define x 
	(lambda (n) 
		(lambda (cc) 
			(cc n) )))

(display (call-with-current-continuation (x 1))) 


(define fact
	(lambda (n pro)
		(if (zero? n)
			pro
			(fact (- n 1) (* pro n))
		)
	)
)

(display (fact 5 1))

(define f
	(lambda (x)
			(define g
				(lambda (y)
				(+ x y)) 
			)
			(g x)
		))
(display (f 1))

(define f
	(lambda (x)
			(
				lambda (y)
				(+ x y)
			)
		))
(display ((f 1) 2))

(define f2 (f 2))
(display  (f2 3))
(display  (f2 4))

(define g
	(lambda (n)
		(* n n)))

(define rep
	(lambda (g n arg)
		(if (- n 1)
			(g (rep g (- n 1) arg))
			(g arg))))

(display (rep g 5 5))


(define caught2 0)
(display (call-with-current-continuation
	(lambda (cc)
		(set! caught2 cc)
		(+ (cc 2) 1))))

(define caught3 0)
(display (call-with-current-continuation
	(lambda (cc)
		(+ (cc 3) (set! caught2 cc) 1))))


(define caught 0)
(define cnt 0)

(display (call-with-current-continuation 
	(lambda (cc)
		(set! caught cc)
		0)) )

(set! cnt (+ cnt 1))

(if (> cnt 10)
	(display "done!")
	(caught cnt))
; (caught cnt)
; (begin (display "Begin a New Journey") (display "That's Right"))
