
(define-syntax begin ()
  [(_ e) e] ; a small optimization
  [(_ e ...) ((lambda () e ...))])

(define-syntax let ()
  [(_ () e ...)
   (begin e ...)] ; workaround
  [(_ ((n v) ...) e ...)
   ((lambda (n ...) e ...) v ...)]
  [(_ name () e ...) ; another workaround
   (begin
     (define (name) e ...)
     (name))]
  [(_ name ((n v) ...) e ...)
   (begin
     (define (name n ...) e ...)
     (name v ...))])

(define-syntax let* ()
  [(_ () e ...)
   (let () e ...)]
  [(_ (b1 b2 ...) e ...)
   (let (b1)
     (let* (b2 ...) e ...))])

(define-syntax and ()
  [(_) #t]
  [(_ x) x]
  [(_ x y ...)
   (if x (and y ...) #f)])

(define-syntax or ()
  [(_) #f]
  [(_ x) x]
  [(_ x y ...)
   (let ([**temp** x])
     (if **temp** **temp** (or y ...)))])

(define-syntax when ()
  [(_ x e ...)
   (if x (begin e ...))])

(define-syntax unless ()
  [(_ x e ...)
   (when (not x) e ...)])

(define-syntax cond (else =>)
  [(_ [else result1 result2 ...])
   (begin result1 result2 ...)]
  [(_ [test => result])
   (let ([temp test])
     (if temp (result temp)))]
  [(_ [test => result] clause1 clause2 ...)
   (let ([**temp** test])
     (if **temp** (result **temp**)
       (cond clause1 clause2 ...)))]
  [(_ [test]) test]
  [(_ [test] clause1 clause2 ...)
   (let ([**temp** test])
     (if **temp** **temp**
       (cond clause1 clause2 ...)))]
  [(_ [test result1 result2 ...])
   (if test (begin result1 result2 ...))]
  [(_ [test result1 result2 ...] clause1 clause2 ...)
   (if test (begin result1 result2 ...)
     (cond clause1 clause2 ...))])

(define-syntax case (else)
  [(_ (key ...)
     clauses ...)
   (let ([atom-key (key ...)])
     (case atom-key clauses ...))]
  [(_ key
     [else result1 result2 ...])
   (begin result1 result2 ...)]
  [(_ key
     [(atoms ...) result1 result2 ...])
   (if (memv key '(atoms ...))
     (begin result1 result2 ...))]
  [(_ key
     [(atoms ...) result1 result2 ...]
     clause clauses ...)
   (if (memv key '(atoms ...))
     (begin result1 result2 ...)
     (case key clause clauses ...))])

(define (x< y) (lambda (x) (< x y)))
(define (x> y) (lambda (x) (> x y)))
(define <x x>)
(define >x x<)
; (define (memv x ls)
;   (if (null? ls) #f
;     (if (eq? (car ls) x) ls
;       (memv x (cdr ls)))))
(define (pretty-print x) (display x))


(begin
  (define (e? ep op x) (or (= x 0) (op ep op (- x 1))))
  (define (o? ep op x) (and (> x 0) (ep ep op (- x 1))))
  (pretty-print (e? e? o? 1000001))
  (pretty-print (o? e? o? 2000000))
  )

; (define (filter f l)
; 	(if (null? l) l
; 		(let ([a (car l)] [l (filter f (cdr l))])
; 			(if (f a) (cons a l) l))))

; (define (append l1 l2)
; 	(if (null? l1) l2
; 		(if (null? l2) l1
; 			(cons (car l1) (append (cdr l1) l2)))))

; (define (sort l)
;   (if (null? l) l
;     (let ([p (car l)])
;     	(append 
;     		(append 
;     			(sort (filter (x< p) l)) 
;     			(filter (lambda (x) (= x p)) l))
;     		(sort (filter (x> p) l)))))))
; (pretty-print (sort '(3 5 4 2 6 3 1 7 8 3 2 9 4 9 0 6 3 2 6 7 4 2 5 7 3 5 2 8 9 5)))

; (let* ((yin
;          ((lambda (cc) (display #\@) cc) (call-with-current-continuation (lambda (c) c))))
;        (yang
;          ((lambda (cc) (display #\*) cc) (call-with-current-continuation (lambda (c) c)))))
;     (yin yang))

; (let loop ([tr '(e . ((a . b) . (c . d)))] [fc (lambda _ #f)])
;  (if (pair? tr)
;    (loop (car tr) (lambda _ (loop (cdr tr) fc)))
;    (fc (display tr))))

; (display (mcar (mcons 1 2)))
; (display (memv (quote car) (quote (car))))

; (display (mcdr (mcons 1 2))) ; 2
; (display (mcons 1 2))

; (display
;   ((lambda (n)
;      ((lambda (f n) (f f n))
;       (lambda (f n)
;         (if (= n 0) 1
;           (* n (f f (- n 1))))) n)) 10))
