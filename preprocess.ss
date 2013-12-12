
(define *stx* (make-eq-hashtable))
(define *void* (void))
(define (*id* x) x)

(define-syntax assert
  (syntax-rules ()
    [(_ x) (unless x
             (error 'assert "Assertion failed" 'x))]))

(define-syntax debug
  (syntax-rules ()
    [(_ ...) (void)]
    [(_) (void)]
    [(_ x) (begin
             (display "***")
             (pretty-print 'x)
             (pretty-print x))]
    [(_ x y ...) (begin (debug x) (debug y ...))]))

(define-syntax debug1
  (syntax-rules ()
    [(_) (void)]
    [(_ x) (begin
             (display "***")
             (pretty-print 'x)
             (pretty-print x))]
    [(_ x y ...) (begin (debug1 x) (debug1 y ...))]))

(define (preprocess lst)
  (define (analyze ls pat? sp?)
    ; 'p => pair
    ; ... => block ...
    ; #t/#f => (sp? atom)
    (if (pair? ls)
      (let ([x (analyze (car ls) pat? sp?)] [ls (cdr ls)])
        (if (and (pair? ls) (eq? (car ls) '...))
          (if pat?  `(... . ,x)
            (cons* 'p `(... . ,x) (analyze (cdr ls) #f sp?)))
          (cons* 'p x (analyze ls pat? sp?))))
      (if (null? ls) ls
        (cons (sp? ls) ls))))

  (define (process-pattern keywords)
    (lambda (entry)
      (define var '())
      (define pattern
        (analyze (cdar entry) #t
                 (lambda (x)
                   (or (not (symbol? x))
                       (and (memq x keywords) #t)
                       (begin (set! var (cons x var)) #f)))))
      (define output
        (analyze (cdr entry) #f
                 (lambda (x) (and (memq x var) #t))))
      (cons pattern output)))

  (define (pattern-match stx ls)
    (define-record-type ellipsis (fields data))
    (define (ellipsis->string x)
      (cond
        [(pair? x) (cons (ellipsis->string (car x))
                         (ellipsis->string (cdr x)))]
        [(ellipsis? x) (assert (list? (ellipsis-data x)))
         (cons '... (ellipsis->string (ellipsis-data x)))]
        [else x]))
    (define (print-ellipsis x) (pretty-print (ellipsis->string x)))

    (define var
      (let match ([pat (car stx)] [ls ls])
        (if (null? pat) (and (null? ls) ls)
          (let ([t (car pat)] [pat (cdr pat)])
            (case t
              [(p) (let ([ma (match (car pat) (car ls))])
                     (and ma (let ([md (match (cdr pat) (cdr ls))])
                               (and md (append ma md)))))]
              [(...) (and (list? ls)
                          (if (and (null? ls) (not (car pat)))
                            (list (cons (cdr pat) (make-ellipsis ls)))
                            (let loop ([ls (reverse ls)] [var '()])
                              (if (null? ls)
                                (map (lambda (e) (cons (car e) (make-ellipsis (cdr e)))) var)
                                (let ([x (match pat (car ls))])
                                  (and x (loop (cdr ls)
                                               (if (null? var)
                                                 (map (lambda (e) (list (car e) (cdr e))) x)
                                                 (map (lambda (e1 e2)
                                                        (define name (car e1))
                                                        (assert (eq? name (car e2)))
                                                        (let* ([v1 (cdr e1)] [v2 (cdr e2)] [v (car v1)])
                                                          (cons* name v2
                                                                 (if (not (ellipsis? v)) v1
                                                                   (begin
                                                                     (assert (ellipsis? v2))
                                                                     (cons (ellipsis-data v) (cdr v1)))))))
                                                      var x)))))))))]
              [(#t) (and (eq? pat ls) '())]
              [(#f) `((,pat . ,ls))])))))
    (and var
         (let output ([out (cdr stx)])
           (if (null? out) out
             (let ([t (car out)] [out (cdr out)])
               (case t
                 [(p) (let ([a (car out)] [d (cdr out)])
                        (let ([oa (output a)] [od (output d)])
                          ((if (and (pair? a) (eq? (car a) '...)) append
                             cons) oa od)))]
                 [(...) (let loop ([x (output out)] [out '()])
                          (let ([has-car? #f] [has-null? #f] [ls '()] [x* #f])
                            (if (pair? x)
                              (let loop ([x x] [x** #f])
                                #f
                                )
                              (let ([x (ellipsis-data x)])
                                (if (null? x) (set! has-null? #t)
                                  (begin
                                    (set! has-car? #t)
                                    (set! ls (car x))
                                    (set! x* (make-ellipsis (cdr x)))))))
                            (if has-null?
                              (if (not has-car?) (reverse! out)
                                (error 'expand-syntax "wrong length"))
                              (if has-car? (loop x* (cons ls out))
                                (if (null? out) out
                                  (error 'expand-syntax "all constant"))))))]
                 [(#t) (cdr (assq out var))]
                 [(#f) out]))))))

  (if (not (pair? lst)) lst
    (let ([h (car lst)] [ls (cdr lst)])
      (case h
        [(quote) lst]
        [(define-syntax)
         (let ([name (car ls)] [ls (cdr ls)])
           (hashtable-set! *stx* name
                           (map (process-pattern (car ls)) (cdr ls))))]
        [else (let ([stx (hashtable-ref *stx* h #f)])
                (if stx
                  (let loop ([stx stx])
                    (cond
                      [(null? stx) (error 'expand-macro "Invaild syntax" lst)]
                      [(pattern-match (car stx) ls) => car]
                      [else (loop (cdr stx))]))
                  (cons (preprocess h) (preprocess ls))))]))))

(let loop ()
  (let process ([ls (read)])
    (let ([lst (preprocess ls)])
      (if (equal? lst ls)
        (unless (eof-object? ls)
          (unless (eq? ls *void*)
            (write ls)
            (newline))
          (loop))
        (process lst)))))