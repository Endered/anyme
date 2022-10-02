(use util.match)

(define (just x)
  (list x))

(define (none)
  (list))

(define (just? x)
  (and (pair? x)
       (null? (cdr x))))

(define (none? x)
  (null? x))

(define (get-just x)
  (car x))

(define (find-map-just f lst)
  (if (null? lst)
      (none)
      (let ((x (f (car lst))))
	(if (just? x)
	    x
	    (find-map-just f (cdr lst))))))

(define (find-if f list)
  (if (null? list)
      #f
      (let ((v (f (car list))))
	(if v (car list) (find-if f (cdr list))))))

(define (join-string sep strings)
  (define (rec lists)
    (if (null? lists)
	""
	(format #f "~a~a~a" sep (car lists) (rec (cdr lists)))))
  (if (null? strings)
      ""
      (format #f "~a~a" (car strings) (rec (cdr strings)))))

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (remove-last list)
  (reverse (cdr (reverse list))))

(define *cps-conversions* ())
(define *transpiler-temporal-variable* "TRANSPILER-TEMPORAL-VARIABLE")

(define next-temporary-variable
  (let ((num 0))
    (lambda ()
      (let ((res (string->symbol (format #f "~a-~a" *transpiler-temporal-variable* num))))
	(set! num (+ num 1))
	res))))

(define-syntax define-cps-conversion
  (syntax-rules ()
    ((_ (head . args) cont then)
     (set! *cps-conversions*
	   (cons
	    (lambda (expr cont)
	      (match expr
		     (('head . args)
		      (just then))
		     (xs (none))))
	    *cps-conversions*)))))

(define-cps-conversion (lambda args . exprs) cont
  (let ((next (next-temporary-variable)))
    `(,cont (lambda ,(cons next args)
	      ,(convert-local-scope exprs next)))))

(define-cps-conversion (set! var expr) cont
  (convert-cps
   expr
   (let ((x (next-temporary-variable)))
     `(lambda (,x) (,cont (set! ,var ,x))))))

(define-cps-conversion (define var) cont
  `(define var))

(define-cps-conversion (if condition then else) cont
  (let ((next (next-temporary-variable))
	(evaled (next-temporary-variable)))
    `((lambda (,next)
	,(convert-cps
	  condition
	  `(lambda (,evaled)
	     (if ,evaled
		 ,(convert-cps then next)
		 ,(convert-cps else next)))))
      ,cont)))

(define-cps-conversion (call/cc f) cont
  (let ((evaled (next-temporary-variable))
	(next (next-temporary-variable))
	(val (next-temporary-variable)))
    (convert-cps
     f
     `(lambda (,evaled)
	(,evaled ,cont
		 (lambda (,next ,val)
		   (,cont ,val)))))))

(define-cps-conversion (transpiler-ffi x) cont
  `(transpiler-ffi ,x))


(define-cps-conversion (cps-call . expr) cont
  (let ((operator-position (index-of operator-call? expr)))
    (if (null? operator-position)
	`(,cont ,expr)
	(let ((tmp-value (next-temporary-variable)))
	  (convert-cps
	   (nth operator-position expr)
	   `(lambda (,tmp-value)
	      ,(convert-cps `(cps-call
			      ,@(take expr operator-position)
			      ,tmp-value
			      ,@(drop expr (+ 1 operator-position)))
			    cont)))))))

(define-cps-conversion (procedure? x) cont
  `(,cont (procedure? ,x)))

(define (var? expr)
  (symbol? expr))

(define (convert-cps-nil expr cont)
  (if (null? expr)
      (just `(,cont ()))
      (none)))

(define (convert-cps-boolean expr cont)
  (if (boolean? expr)
      (just `(,cont ,expr))
      (none)))

(define (convert-cps-var expr cont)
  (if (var? expr)
      (just `(,cont ,expr))
      (none)))

(define (convert-cps-number expr cont)
  (if (number? expr)
      (just `(,cont ,expr))
      (none)))

(define (convert-cps-string expr cont)
  (if (string? expr)
      (just `(,cont ,expr))
      (none)))

(define (convert-cps-macros expr cont)
  (find-map-just
   (lambda (f)
     (f expr cont))
   *cps-conversions*))

(define (operator-call? expr)
  (pair? expr))

(define (index-of f lst)
  (define (rec lst cnt)
    (cond ((null? lst) ())
	  ((f (car lst)) cnt)
	  (else
	   (rec (cdr lst) (+ cnt 1)))))
  (rec lst 0))

(define (nth n lst)
  (if (<= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(define (convert-cps-function-call expr cont)
  (just 
   (let ((operator-position (index-of operator-call? expr)))
     (if (null? operator-position)
	 `(,(car expr) ,cont ,@(cdr expr))
	 (let ((tmp-value (next-temporary-variable)))
	   (convert-cps
	    (nth operator-position expr)
	    `(lambda (,tmp-value)
	       ,(convert-cps `(,@(take expr operator-position)
			       ,tmp-value
			       ,@(drop expr (+ 1 operator-position)))
			     cont))))))))

(define (convert-global-scope exprs)
  (let ((global-vars (mappend (match-lambda (('define var expr) (list var))
					    (('define var) (list var))
					    (_ ()))
			      exprs))
	(define-removed (mappend (match-lambda (('define var expr) (list `(set! ,var ,expr)))
					       (('define var) ())
					       (x (list x)))
				 exprs)))
    (append (map (lambda (x) `(define ,x)) global-vars)
	    (map (lambda (x) (convert-cps x '(lambda (x) x))) define-removed))))

(define (fold-exprs exprs)
  (match exprs
	 ((x) x)
	 ((x . other)
	  (let ((tmp (next-temporary-variable)))
	    `((lambda (,tmp) ,(fold-exprs other)) ,x)))))

(define (convert-local-scope exprs cont)
  (let ((global-vars (mappend (match-lambda (('define var expr) (list var))
					    (('define var) (list var))
					    (_ ()))
			      exprs))
	(define-removed (mappend (match-lambda (('define var expr) (list `(set! ,var ,expr)))
					       (('define var) ())
					       (x (list x)))
				 exprs)))
    `((lambda ,global-vars ,(convert-cps (fold-exprs define-removed) cont))
      ,@(map (lambda (x) ()) global-vars))))

(define (convert-cps expr env)
  (get-just (find-map-just
	     (lambda (f)
	       (f expr env))
	     (list
	      convert-cps-nil
	      convert-cps-boolean
	      convert-cps-var
	      convert-cps-number
	      convert-cps-string
	      convert-cps-macros
	      convert-cps-function-call))))

(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))

(map (lambda (line)
       (write line)
       (newline))
     (convert-global-scope
      (read-while-eof)))

