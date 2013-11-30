
(define (A k x1 x2 x3 x4 x5)
  (define (B)
    (set! k (- k 1))
    (A k B x1 x2 x3 x4))
  (if (<= k 0)
      (+ (x4) (x5))
      (B)))
 
(define (man-or-boy n)
	(display (A n (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))))


; (display "Show me -67 if you are a MAN:")
(man-or-boy 17)


