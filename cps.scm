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
      (let ((res (string->symbol (format #f "~a_~a" *transpiler-temporal-variable* num))))
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

(define-cps-conversion (lambda args expr) cont
  (let ((next (gensym)))
    `(,cont (lambda ,(cons next args)
	      ,(convert-cps expr next)))))

(define-cps-conversion (set! var expr) cont
  (convert-cps
   expr
   (let ((x (gensym)))
     `(lambda (,x) (,cont (set! ,var ,x))))))

(define-cps-conversion (define var) cont
  `(define var))

(define (var? expr)
  (symbol? expr))

(define (convert-cps-nil expr cont)
  (if (null? expr)
      (just `(,cont ()))
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
	    (map (lambda (x) (convert-cps x 'identity)) define-removed))))

(define (convert-cps expr env)
  (get-just (find-map-just
	     (lambda (f)
	       (f expr env))
	     (list
	      convert-cps-nil
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


(define (=+ cont . args)
  (cont (apply + args)))

(define (=* cont . args)
  (cont (apply * args)))

(print
 (convert-cps
  '(=+ 1 (=+ 2 (=+ 3 4)))
  "identity"))

(print
 (convert-cps
  '(set! square
	 (lambda ()
	   (=* n n)))
  "identity"))

(print
 (convert-cps
  '(set! double
	 (lambda (n)
   (=+ n n)))
  "identity"))

(print
 (convert-cps
  '(lambda (x) (=+ x x))
  "identity"))


(define (cps-function f)
  (lambda (cont . args)
    (cont (apply f args))))

(define =print (cps-function print))

(map
 (lambda (line)
   (display line)
   (newline))
 (convert-global-scope
  '(
    (define square (lambda (x) (=* x x)))
    (=print (square 8))
    )))


(define square)
((lambda (G327) (identity (set! square G327)))
 (lambda (G328 x) (=* G328 x x)))
(square (lambda (TRANSPILER-TEMPORAL-VARIABLE_26)
	  (=print identity TRANSPILER-TEMPORAL-VARIABLE_26))
	8)
